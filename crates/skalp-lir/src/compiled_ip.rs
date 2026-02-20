//! Compiled IP Format
//!
//! Binary format for distributing pre-synthesized gate netlists.
//! Supports optional encryption for IP protection.
//!
//! # File Formats
//!
//! - `.skb` - Compiled binary containing serialized GateNetlist
//! - `.skh` - Header file with entity declarations for import
//!
//! # Example
//!
//! ```ignore
//! use skalp_lir::compiled_ip::{CompiledIp, generate_header};
//!
//! // Compile a design
//! let compiled = CompiledIp::new(gate_netlist, "generic_asic");
//! compiled.write_to_file("design.skb", None)?;
//!
//! // Generate header
//! let header = generate_header(&compiled, "design.skb");
//! std::fs::write("design.skh", header)?;
//! ```

use crate::gate_netlist::GateNetlist;
use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

/// Magic number for .skb files: "SKB\x01" (SKALP Binary v1)
pub const SKB_MAGIC: [u8; 4] = [0x53, 0x4B, 0x42, 0x01];

/// Current format version
pub const SKB_VERSION: u16 = 1;

/// Format markers (byte after magic)
const FORMAT_UNENCRYPTED: u8 = 0x00;
const FORMAT_ENCRYPTED: u8 = 0x01;

/// Header for compiled IP files
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompiledIpHeader {
    /// Format version (for forward compatibility)
    pub version: u16,
    /// Technology library used during synthesis
    pub library_name: String,
    /// SKALP compiler version that created this
    pub compiler_version: String,
    /// Creation timestamp (Unix epoch seconds)
    pub created_at: u64,
    /// SHA-256 hash of the original source (for verification)
    pub source_hash: Option<String>,
    /// Is this file encrypted?
    pub encrypted: bool,
    /// Encryption key ID (if encrypted) - for key management
    pub key_id: Option<String>,
    /// Blake3 hash of the netlist content (after decryption if encrypted)
    pub content_checksum: [u8; 32],
}

impl Default for CompiledIpHeader {
    fn default() -> Self {
        Self {
            version: SKB_VERSION,
            library_name: String::new(),
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            created_at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
            source_hash: None,
            encrypted: false,
            key_id: None,
            content_checksum: [0u8; 32],
        }
    }
}

/// Complete compiled IP container
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompiledIp {
    /// Metadata header
    pub header: CompiledIpHeader,
    /// The gate netlist
    pub netlist: GateNetlist,
    /// Port metadata for type checking (subset of HIR entity info)
    pub port_info: Vec<CompiledPortInfo>,
    /// Generic parameter values used during compilation
    pub generic_values: Vec<(String, GenericValue)>,
}

/// Port information for type checking during import
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompiledPortInfo {
    /// Port name
    pub name: String,
    /// Port direction
    pub direction: CompiledPortDirection,
    /// Bit width of the port
    pub width: u32,
    /// Is this a clock port?
    pub is_clock: bool,
    /// Is this a reset port?
    pub is_reset: bool,
    /// Is this a detection signal (for safety)?
    pub is_detection: bool,
}

/// Port direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompiledPortDirection {
    /// Input port
    Input,
    /// Output port
    Output,
    /// Bidirectional port
    InOut,
}

/// Generic parameter value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GenericValue {
    /// Integer value
    Int(i64),
    /// Unsigned integer value
    Uint(u64),
    /// Boolean value
    Bool(bool),
    /// String value
    String(String),
}

impl CompiledIp {
    /// Create a new compiled IP from a gate netlist
    pub fn new(netlist: GateNetlist, library_name: &str) -> Self {
        // Extract port info from netlist
        let port_info = Self::extract_port_info(&netlist);

        // Compute content checksum
        let checksum = Self::compute_checksum(&netlist);

        Self {
            header: CompiledIpHeader {
                library_name: library_name.to_string(),
                content_checksum: checksum,
                ..Default::default()
            },
            netlist,
            port_info,
            generic_values: Vec::new(),
        }
    }

    /// Create compiled IP with generic values
    pub fn with_generics(mut self, generics: Vec<(String, GenericValue)>) -> Self {
        self.generic_values = generics;
        self
    }

    /// Create compiled IP with source hash
    pub fn with_source_hash(mut self, hash: String) -> Self {
        self.header.source_hash = Some(hash);
        self
    }

    /// Write to file (optionally encrypted)
    ///
    /// # Arguments
    /// * `path` - Output file path
    /// * `key` - Optional 32-byte AES-256 key for encryption
    pub fn write_to_file(&self, path: &Path, key: Option<&[u8; 32]>) -> Result<()> {
        let file = File::create(path).context("Failed to create output file")?;
        let mut writer = BufWriter::new(file);

        // Write magic number
        writer.write_all(&SKB_MAGIC)?;

        if let Some(encryption_key) = key {
            #[cfg(feature = "encryption")]
            {
                // Write format marker
                writer.write_all(&[FORMAT_ENCRYPTED])?;

                // Encrypt and write
                let mut compiled = self.clone();
                compiled.header.encrypted = true;

                // Serialize the content (netlist + port_info + generic_values)
                let content = bincode::serialize(&(
                    &compiled.netlist,
                    &compiled.port_info,
                    &compiled.generic_values,
                ))?;

                // Encrypt
                let encrypted = encryption::encrypt(&content, encryption_key)?;

                // Write header separately (unencrypted for inspection)
                let header_bytes = bincode::serialize(&compiled.header)?;
                let header_len = header_bytes.len() as u32;
                writer.write_all(&header_len.to_le_bytes())?;
                writer.write_all(&header_bytes)?;

                // Write encrypted content
                writer.write_all(&encrypted)?;
            }
            #[cfg(not(feature = "encryption"))]
            {
                bail!("Encryption support not enabled. Rebuild with --features encryption");
            }
        } else {
            // Write format marker
            writer.write_all(&[FORMAT_UNENCRYPTED])?;

            // Write unencrypted
            let bytes = bincode::serialize(self)?;
            writer.write_all(&bytes)?;
        }

        writer.flush()?;
        Ok(())
    }

    /// Read from file (optionally decrypt)
    ///
    /// # Arguments
    /// * `path` - Input file path
    /// * `key` - Optional 32-byte AES-256 key for decryption
    pub fn read_from_file(path: &Path, key: Option<&[u8; 32]>) -> Result<Self> {
        let file = File::open(path).context("Failed to open input file")?;
        let mut reader = BufReader::new(file);

        // Read and verify magic number
        let mut magic = [0u8; 4];
        reader.read_exact(&mut magic)?;
        if magic != SKB_MAGIC {
            bail!(
                "Invalid .skb file: bad magic number (expected {:?}, got {:?})",
                SKB_MAGIC,
                magic
            );
        }

        // Read format marker
        let mut format_byte = [0u8; 1];
        reader.read_exact(&mut format_byte)?;

        // Read rest of file
        let mut data = Vec::new();
        reader.read_to_end(&mut data)?;

        match format_byte[0] {
            FORMAT_ENCRYPTED => {
                #[cfg(feature = "encryption")]
                {
                    let key = key
                        .ok_or_else(|| anyhow::anyhow!("Encrypted file requires decryption key"))?;

                    // Read header length
                    if data.len() < 4 {
                        bail!("Invalid encrypted .skb file: too short");
                    }
                    let header_len =
                        u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as usize;

                    if data.len() < 4 + header_len {
                        bail!("Invalid encrypted .skb file: header truncated");
                    }

                    // Deserialize header
                    let header: CompiledIpHeader = bincode::deserialize(&data[4..4 + header_len])
                        .context("Failed to deserialize header")?;

                    // Decrypt content
                    let encrypted_content = &data[4 + header_len..];
                    let decrypted = encryption::decrypt(encrypted_content, key)?;

                    let (mut netlist, port_info, generic_values): (
                        GateNetlist,
                        Vec<CompiledPortInfo>,
                        Vec<(String, GenericValue)>,
                    ) = bincode::deserialize(&decrypted)
                        .context("Failed to deserialize decrypted content")?;

                    // Verify checksum
                    let checksum = Self::compute_checksum(&netlist);
                    if checksum != header.content_checksum {
                        bail!("Content checksum mismatch - file may be corrupted");
                    }

                    // Rebuild caches that were skipped during serialization
                    netlist.rebuild_cache();

                    Ok(Self {
                        header,
                        netlist,
                        port_info,
                        generic_values,
                    })
                }
                #[cfg(not(feature = "encryption"))]
                {
                    bail!(
                        "File is encrypted but encryption support not enabled. \
                         Rebuild with --features encryption"
                    );
                }
            }
            FORMAT_UNENCRYPTED => {
                // Unencrypted format - deserialize directly
                let mut compiled: CompiledIp =
                    bincode::deserialize(&data).context("Failed to deserialize compiled IP")?;

                // Verify checksum
                let checksum = Self::compute_checksum(&compiled.netlist);
                if checksum != compiled.header.content_checksum {
                    bail!("Content checksum mismatch - file may be corrupted");
                }

                // Rebuild caches that were skipped during serialization
                compiled.netlist.rebuild_cache();

                Ok(compiled)
            }
            other => {
                bail!(
                    "Unknown .skb format marker: 0x{:02X} (expected 0x00 for unencrypted or 0x01 for encrypted)",
                    other
                );
            }
        }
    }

    /// Extract port information from a gate netlist
    fn extract_port_info(netlist: &GateNetlist) -> Vec<CompiledPortInfo> {
        let mut ports = Vec::new();

        // Inputs
        for &net_id in &netlist.inputs {
            if let Some(net) = netlist.nets.get(net_id.0 as usize) {
                ports.push(CompiledPortInfo {
                    name: net.name.clone(),
                    direction: CompiledPortDirection::Input,
                    width: 1, // Gate netlist is bit-blasted
                    is_clock: false,
                    is_reset: false,
                    is_detection: false,
                });
            }
        }

        // Clocks (special inputs)
        for &net_id in &netlist.clocks {
            if let Some(net) = netlist.nets.get(net_id.0 as usize) {
                // Find and update the existing port or add new one
                if let Some(port) = ports.iter_mut().find(|p| p.name == net.name) {
                    port.is_clock = true;
                } else {
                    ports.push(CompiledPortInfo {
                        name: net.name.clone(),
                        direction: CompiledPortDirection::Input,
                        width: 1,
                        is_clock: true,
                        is_reset: false,
                        is_detection: false,
                    });
                }
            }
        }

        // Resets (special inputs)
        for &net_id in &netlist.resets {
            if let Some(net) = netlist.nets.get(net_id.0 as usize) {
                if let Some(port) = ports.iter_mut().find(|p| p.name == net.name) {
                    port.is_reset = true;
                } else {
                    ports.push(CompiledPortInfo {
                        name: net.name.clone(),
                        direction: CompiledPortDirection::Input,
                        width: 1,
                        is_clock: false,
                        is_reset: true,
                        is_detection: false,
                    });
                }
            }
        }

        // Outputs
        for &net_id in &netlist.outputs {
            if let Some(net) = netlist.nets.get(net_id.0 as usize) {
                ports.push(CompiledPortInfo {
                    name: net.name.clone(),
                    direction: CompiledPortDirection::Output,
                    width: 1,
                    is_clock: false,
                    is_reset: false,
                    is_detection: false,
                });
            }
        }

        ports
    }

    /// Compute Blake3 hash of the netlist for integrity checking
    fn compute_checksum(netlist: &GateNetlist) -> [u8; 32] {
        let serialized = bincode::serialize(netlist).unwrap_or_default();
        blake3::hash(&serialized).into()
    }

    /// Verify the content checksum
    pub fn verify_checksum(&self) -> bool {
        let computed = Self::compute_checksum(&self.netlist);
        computed == self.header.content_checksum
    }

    /// Get a summary of this compiled IP
    pub fn summary(&self) -> String {
        format!(
            "CompiledIp '{}': {} cells, {} nets, {} ports (lib: {}, v{})",
            self.netlist.name,
            self.netlist.cells.len(),
            self.netlist.nets.len(),
            self.port_info.len(),
            self.header.library_name,
            self.header.version
        )
    }
}

/// Generate a .skh header file from compiled IP
///
/// # Arguments
/// * `compiled` - The compiled IP
/// * `skb_path` - Relative path to the .skb file (for the #[compiled_ip] attribute)
pub fn generate_header(compiled: &CompiledIp, skb_path: &str) -> String {
    let mut output = String::new();

    // Header comment
    output.push_str(&format!(
        "// Auto-generated header for compiled IP: {}\n",
        compiled.netlist.name
    ));
    output.push_str(&format!("// Library: {}\n", compiled.header.library_name));

    // Format timestamp
    let timestamp = compiled.header.created_at;
    output.push_str(&format!("// Compiled: {} (Unix timestamp)\n", timestamp));

    if let Some(ref hash) = compiled.header.source_hash {
        output.push_str(&format!("// Source hash: {}\n", hash));
    }

    output.push('\n');

    // Entity declaration with compiled_ip attribute
    output.push_str(&format!("#[compiled_ip(\"{}\")]\n", skb_path));

    if compiled.header.encrypted {
        output.push_str("#[encrypted]\n");
    }

    output.push_str(&format!("entity {} {{\n", compiled.netlist.name));

    // Group ports by direction
    let inputs: Vec<_> = compiled
        .port_info
        .iter()
        .filter(|p| p.direction == CompiledPortDirection::Input)
        .collect();
    let outputs: Vec<_> = compiled
        .port_info
        .iter()
        .filter(|p| p.direction == CompiledPortDirection::Output)
        .collect();
    let inouts: Vec<_> = compiled
        .port_info
        .iter()
        .filter(|p| p.direction == CompiledPortDirection::InOut)
        .collect();

    // Write inputs
    for port in inputs {
        let type_str = port_type_string(port);
        let attrs = port_attributes(port);
        if !attrs.is_empty() {
            output.push_str(&format!("    {}\n", attrs));
        }
        output.push_str(&format!("    in {}: {},\n", port.name, type_str));
    }

    // Write outputs
    for port in outputs {
        let type_str = port_type_string(port);
        let attrs = port_attributes(port);
        if !attrs.is_empty() {
            output.push_str(&format!("    {}\n", attrs));
        }
        output.push_str(&format!("    out {}: {},\n", port.name, type_str));
    }

    // Write inouts
    for port in inouts {
        let type_str = port_type_string(port);
        let attrs = port_attributes(port);
        if !attrs.is_empty() {
            output.push_str(&format!("    {}\n", attrs));
        }
        output.push_str(&format!("    inout {}: {},\n", port.name, type_str));
    }

    // Remove trailing comma if present
    if output.ends_with(",\n") {
        output.pop();
        output.pop();
        output.push('\n');
    }

    output.push_str("}\n");

    output
}

/// Get the SKALP type string for a port
fn port_type_string(port: &CompiledPortInfo) -> String {
    if port.is_clock {
        "clock".to_string()
    } else if port.is_reset {
        "reset".to_string()
    } else if port.width == 1 {
        "bit".to_string()
    } else {
        format!("bit<{}>", port.width)
    }
}

/// Get attribute strings for a port
fn port_attributes(port: &CompiledPortInfo) -> String {
    let mut attrs = Vec::new();

    if port.is_detection {
        attrs.push("#[detection_signal]".to_string());
    }

    attrs.join(" ")
}

// Encryption support (feature-gated)
#[cfg(feature = "encryption")]
mod encryption {
    use aes_gcm::{
        aead::{Aead, KeyInit},
        Aes256Gcm, Key, Nonce,
    };
    use anyhow::{bail, Result};
    use rand::RngCore;

    /// Encrypt data using AES-256-GCM
    pub fn encrypt(data: &[u8], key: &[u8; 32]) -> Result<Vec<u8>> {
        let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(key));

        // Generate random nonce
        let mut nonce_bytes = [0u8; 12];
        rand::thread_rng().fill_bytes(&mut nonce_bytes);
        let nonce = Nonce::from_slice(&nonce_bytes);

        let ciphertext = cipher
            .encrypt(nonce, data)
            .map_err(|e| anyhow::anyhow!("Encryption failed: {}", e))?;

        // Prepend nonce to ciphertext
        let mut result = nonce_bytes.to_vec();
        result.extend(ciphertext);
        Ok(result)
    }

    /// Decrypt data using AES-256-GCM
    pub fn decrypt(data: &[u8], key: &[u8; 32]) -> Result<Vec<u8>> {
        if data.len() < 12 {
            bail!("Invalid encrypted data: too short for nonce");
        }

        let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(key));
        let (nonce_bytes, ciphertext) = data.split_at(12);
        let nonce = Nonce::from_slice(nonce_bytes);

        cipher
            .decrypt(nonce, ciphertext)
            .map_err(|e| anyhow::anyhow!("Decryption failed: {}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{GateNet, GateNetId};

    fn create_test_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test_ip".to_string(), "generic_asic".to_string());

        // Add some test nets
        let clk = netlist.add_input("clk".to_string());
        let rst = netlist.add_input("rst".to_string());
        let data_in = netlist.add_input("data_in".to_string());
        let data_out = netlist.add_output("data_out".to_string());

        // Mark clk as clock
        netlist.clocks.push(clk);
        // Mark rst as reset
        netlist.resets.push(rst);

        netlist
    }

    #[test]
    fn test_compiled_ip_creation() {
        let netlist = create_test_netlist();
        let compiled = CompiledIp::new(netlist, "generic_asic");

        assert_eq!(compiled.header.version, SKB_VERSION);
        assert_eq!(compiled.header.library_name, "generic_asic");
        assert!(!compiled.header.encrypted);
        assert!(compiled.verify_checksum());
    }

    #[test]
    fn test_port_extraction() {
        let netlist = create_test_netlist();
        let compiled = CompiledIp::new(netlist, "generic_asic");

        // Should have 4 ports: clk, rst, data_in, data_out
        assert_eq!(compiled.port_info.len(), 4);

        // Check clk is marked as clock
        let clk_port = compiled.port_info.iter().find(|p| p.name == "clk").unwrap();
        assert!(clk_port.is_clock);
        assert_eq!(clk_port.direction, CompiledPortDirection::Input);

        // Check rst is marked as reset
        let rst_port = compiled.port_info.iter().find(|p| p.name == "rst").unwrap();
        assert!(rst_port.is_reset);

        // Check data_out is output
        let out_port = compiled
            .port_info
            .iter()
            .find(|p| p.name == "data_out")
            .unwrap();
        assert_eq!(out_port.direction, CompiledPortDirection::Output);
    }

    #[test]
    fn test_header_generation() {
        let netlist = create_test_netlist();
        let compiled = CompiledIp::new(netlist, "generic_asic");
        let header = generate_header(&compiled, "test_ip.skb");

        assert!(header.contains("#[compiled_ip(\"test_ip.skb\")]"));
        assert!(header.contains("entity test_ip"));
        assert!(header.contains("in clk: clock"));
        assert!(header.contains("in rst: reset"));
        assert!(header.contains("out data_out: bit"));
    }

    #[test]
    fn test_roundtrip_unencrypted() {
        use tempfile::TempDir;

        let netlist = create_test_netlist();
        let compiled = CompiledIp::new(netlist, "generic_asic");

        let temp_dir = TempDir::new().unwrap();
        let skb_path = temp_dir.path().join("test.skb");

        // Write
        compiled.write_to_file(&skb_path, None).unwrap();

        // Read back
        let loaded = CompiledIp::read_from_file(&skb_path, None).unwrap();

        assert_eq!(loaded.netlist.name, compiled.netlist.name);
        assert_eq!(loaded.port_info.len(), compiled.port_info.len());
        assert!(loaded.verify_checksum());
    }

    #[test]
    fn test_checksum_verification() {
        let netlist = create_test_netlist();
        let mut compiled = CompiledIp::new(netlist, "generic_asic");

        // Valid checksum
        assert!(compiled.verify_checksum());

        // Corrupt checksum
        compiled.header.content_checksum[0] ^= 0xFF;
        assert!(!compiled.verify_checksum());
    }

    #[test]
    fn test_summary() {
        let netlist = create_test_netlist();
        let compiled = CompiledIp::new(netlist, "generic_asic");
        let summary = compiled.summary();

        assert!(summary.contains("test_ip"));
        assert!(summary.contains("generic_asic"));
    }
}
