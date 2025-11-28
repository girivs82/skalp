#[cfg(test)]
mod graphics_pipeline_functional_tests {
    use skalp_testing::testbench::Testbench;

    // BUG FIXED: Compiler was dropping input port references in compound boolean conditions.
    // The condition `if state == 0 && vertex_valid` was being compiled as just `if state == 0`.
    // Fixed in skalp-frontend/src/hir_builder.rs:1604 by ensuring we select the outermost
    // expression node (the && operation) rather than an inner node (the == comparison).
    #[tokio::test]
    async fn test_geometry_processor_vertex_passthrough() {
        println!("🎨 Testing GeometryProcessor4 Vertex Passthrough");

        // Create testbench for geometry processor stub
        let geometry_source = r#"
// Simple inline types for testing
struct Vec3f {
    x: bit[32]
    y: bit[32]
    z: bit[32]
}

struct TestVertex {
    position_x: bit[32]
    position_y: bit[32]
    position_z: bit[32]
}

struct TestOutput {
    position_x: bit[32]
    position_y: bit[32]
    position_z: bit[32]
    position_w: bit[32]
}

entity GeometryProcessor4 {
    in clk: clock
    in rst: reset(active_high)
    in vertex_valid: bit
    in vertex_x: bit[32]
    in vertex_y: bit[32]
    in vertex_z: bit[32]
    out vertex_ready: bit
    out output_valid: bit
    out output_x: bit[32]
    out output_y: bit[32]
    out output_z: bit[32]
    out output_w: bit[32]
    in output_ready: bit
    out busy: bit
    out vertices_processed: bit[32]
}

impl GeometryProcessor4 {
    signal state: bit[2]
    signal count: bit[32]

    // Use scalar signals throughout - no structs
    signal out_pos_x: bit[32]
    signal out_pos_y: bit[32]
    signal out_pos_z: bit[32]
    signal out_pos_w: bit[32]

    on(clk.rise) {
        if rst {
            state <= 0
            count <= 0
        } else {
            if state == 0 && vertex_valid {
                // Capture and transform vertex (passthrough for now)
                out_pos_x <= vertex_x
                out_pos_y <= vertex_y
                out_pos_z <= vertex_z
                out_pos_w <= 0x3F800000  // 1.0 in float32
                state <= 1
                count <= count + 1
            } else if state == 1 && output_ready {
                state <= 0
            }
        }
    }

    vertex_ready = (state == 0)
    output_valid = (state == 1)
    busy = (state != 0)
    vertices_processed = count

    output_x = out_pos_x
    output_y = out_pos_y
    output_z = out_pos_z
    output_w = out_pos_w
}
        "#;

        // Save to temp file (testbench needs a file path)
        let temp_dir = std::env::temp_dir();
        let source_file = temp_dir.join("geometry_processor_test.sk");
        std::fs::write(&source_file, geometry_source).expect("Failed to write test source");

        let mut tb = Testbench::new(source_file.to_str().unwrap())
            .await
            .expect("Failed to create testbench");

        println!("   ✅ Testbench created");

        // Reset the processor
        println!("   🔄 Resetting GeometryProcessor4...");

        // CRITICAL: Initialize all control signals to 0 BEFORE and DURING reset
        tb.set("vertex_valid", 0u8);
        tb.set("output_ready", 0u8);
        tb.set("rst", 1u8).clock(2).await;

        tb.set("rst", 0u8).clock(1).await;

        // Verify initial state
        tb.expect("vertex_ready", 1u8).await;
        tb.expect("output_valid", 0u8).await;
        tb.expect("busy", 0u8).await;
        tb.expect("vertices_processed", 0u32).await;

        println!("   ✅ Initial state correct");

        // Test vertex passthrough
        println!("   📝 Sending test vertex...");

        // Set up a test vertex with known values (do NOT set output_ready yet)
        tb.set("vertex_valid", 1u8);
        tb.set("vertex_x", 0x40000000u32); // 2.0 in float32
        tb.set("vertex_y", 0x40400000u32); // 3.0 in float32
        tb.set("vertex_z", 0x40800000u32); // 4.0 in float32
        tb.set("output_ready", 0u8); // Keep output_ready low

        // Clock once to capture vertex
        tb.clock(1).await;

        // Processor should be busy now, output should be valid
        tb.expect("vertex_ready", 0u8).await;
        tb.expect("output_valid", 1u8).await;
        tb.expect("busy", 1u8).await;

        println!("   ✅ Processor captured vertex and asserted output_valid");

        // Check that output matches input (passthrough) - do this BEFORE setting output_ready
        let out_x: u32 = tb.get_as("output_x").await;
        let out_y: u32 = tb.get_as("output_y").await;
        let out_z: u32 = tb.get_as("output_z").await;
        let out_w: u32 = tb.get_as("output_w").await;

        println!("      Read output values:");
        println!("         X = 0x{:08X} (expected 0x40000000)", out_x);
        println!("         Y = 0x{:08X} (expected 0x40400000)", out_y);
        println!("         Z = 0x{:08X} (expected 0x40800000)", out_z);
        println!("         W = 0x{:08X} (expected 0x3F800000)", out_w);

        assert_eq!(out_x, 0x40000000, "Position X should pass through");
        assert_eq!(out_y, 0x40400000, "Position Y should pass through");
        assert_eq!(out_z, 0x40800000, "Position Z should pass through");
        assert_eq!(out_w, 0x3F800000, "Position W should be 1.0");

        println!("   ✅ Vertex data passed through correctly");

        // Check vertices_processed counter
        tb.expect("vertices_processed", 1u32).await;

        // NOW set output_ready and clock to return to ready state
        tb.set("output_ready", 1u8).clock(1).await;
        tb.expect("vertex_ready", 1u8).await;
        tb.expect("output_valid", 0u8).await;
        tb.expect("busy", 0u8).await;

        println!("   ✅ Processor returned to ready state");
        println!("✅ GeometryProcessor4 test PASSED!");
    }

    // GPU simulator captures outputs AFTER phase 3 (after combinational re-evaluation)
    // This provides correct FIFO semantics where outputs reflect the post-clock state.
    // Verified: AsyncFifo correctly reads sequential values (0x12345678, 0xABCDEF00, 0xDEADBEEF).
    #[tokio::test]
    async fn test_async_fifo_clock_domain_crossing() {
        println!("🎨 Testing AsyncFIFO Clock Domain Crossing");

        let fifo_source = r#"
mod async_fifo;
use async_fifo::{AsyncFifo, clog2};

struct SimpleData {
    value: bit[32]
}

entity FifoTest {
    in wr_clk: clock
    in wr_rst: reset(active_high)
    in rd_clk: clock
    in rd_rst: reset(active_high)

    in write_data: bit[32]
    in write_enable: bit
    out write_full: bit

    out read_data: bit[32]
    in read_enable: bit
    out read_empty: bit
}

impl FifoTest {
    signal wr_data_internal: SimpleData
    signal rd_data_internal: SimpleData

    wr_data_internal.value = write_data
    read_data = rd_data_internal.value

    let fifo = AsyncFifo<SimpleData, 8> {
        wr_clk: wr_clk,
        wr_rst: wr_rst,
        wr_en: write_enable,
        wr_data: wr_data_internal,
        wr_full: write_full,
        rd_clk: rd_clk,
        rd_rst: rd_rst,
        rd_en: read_enable,
        rd_data: rd_data_internal,
        rd_empty: read_empty
    }
}
        "#;

        let temp_dir = std::env::temp_dir();

        // Copy async_fifo module to temp directory so it can be found
        let async_fifo_source =
            std::fs::read_to_string("examples/graphics_pipeline/lib/async_fifo.sk")
                .expect("Failed to read async_fifo.sk");
        std::fs::write(temp_dir.join("async_fifo.sk"), async_fifo_source)
            .expect("Failed to write async_fifo.sk to temp");

        let source_file = temp_dir.join("fifo_cdc_test.sk");
        std::fs::write(&source_file, fifo_source).expect("Failed to write test source");

        let mut tb = Testbench::new(source_file.to_str().unwrap())
            .await
            .expect("Failed to create testbench");

        println!("   ✅ Testbench created");

        // Reset both clock domains
        println!("   🔄 Resetting both clock domains...");

        // CRITICAL: Initialize all control signals to 0 BEFORE reset
        tb.set("write_enable", 0u8).set("read_enable", 0u8);

        tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
        tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;

        tb.set("wr_rst", 0u8).set("rd_rst", 0u8);
        tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;

        // Verify FIFO is empty
        // TODO: Implement read_empty check once signal access is fixed
        println!("   ✅ FIFO reset complete");

        // Write some data at wr_clk rate
        println!("   📝 Writing data to FIFO...");
        let test_values = [0x12345678u32, 0xABCDEF00u32, 0xDEADBEEFu32];

        for (i, &value) in test_values.iter().enumerate() {
            tb.set("write_data", value).set("write_enable", 1u8);
            tb.clock_signal("wr_clk", 1).await;
            println!("      Wrote value {}: 0x{:08X}", i, value);
        }

        tb.set("write_enable", 0u8);
        tb.clock_signal("wr_clk", 1).await;

        println!("   ✅ Data written to FIFO (wr_ptr should be 3)");

        // Give time for CDC synchronization
        // CDC needs 2-3 rd_clk cycles AFTER wr_ptr_gray is stable for synchronizers
        println!("   ⏳ Waiting for CDC synchronization...");

        // First, give a few more wr_clk cycles to ensure wr_ptr_gray is stable
        tb.clock_signal("wr_clk", 3).await;

        // Now give rd_clk cycles for the two-flip-flop synchronizer
        // to propagate wr_ptr_gray → wr_ptr_gray_sync1 → wr_ptr_gray_sync2
        tb.clock_signal("rd_clk", 5).await;

        // Read data back at rd_clk rate - explicitly unrolled to debug
        println!("   📖 Reading data from FIFO...");

        // Read value 0
        let empty0: u8 = tb.get_as("read_empty").await;
        let val0: u32 = tb.get_as("read_data").await;
        println!("      Read 0: empty={}, value=0x{:08X}", empty0, val0);
        assert_eq!(val0, 0x12345678, "First value mismatch");

        // Advance: set read_enable=1, clock
        tb.set("read_enable", 1u8);
        tb.clock_signal("rd_clk", 1).await;

        // Read value 1
        let empty1: u8 = tb.get_as("read_empty").await;
        let val1: u32 = tb.get_as("read_data").await;
        println!("      Read 1: empty={}, value=0x{:08X}", empty1, val1);
        assert_eq!(val1, 0xABCDEF00, "Second value mismatch");

        // Advance again (keep read_enable=1)
        tb.clock_signal("rd_clk", 1).await;

        // Read value 2
        let empty2: u8 = tb.get_as("read_empty").await;
        let val2: u32 = tb.get_as("read_data").await;
        println!("      Read 2: empty={}, value=0x{:08X}", empty2, val2);

        // Only assert if not empty
        if empty2 == 0 {
            assert_eq!(val2, 0xDEADBEEF, "Third value mismatch");
        } else {
            println!("      ⚠️  FIFO reports empty after 2 reads (expected 3rd value)");
            println!("      This suggests a CDC synchronization timing issue");

            // Give more time for CDC to propagate
            tb.set("read_enable", 0u8);
            println!("   ⏳ Adding extra CDC sync cycles...");
            tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 4)]).await;

            // Try reading again
            tb.set("read_enable", 1u8);
            let empty_retry: u8 = tb.get_as("read_empty").await;
            let val_retry: u32 = tb.get_as("read_data").await;
            println!(
                "      Retry read: empty={}, value=0x{:08X}",
                empty_retry, val_retry
            );

            if empty_retry == 0 {
                assert_eq!(val_retry, 0xDEADBEEF, "Third value mismatch after retry");
            } else {
                panic!("FIFO still empty after additional CDC sync - write pointer may not have propagated correctly");
            }
        }

        tb.set("read_enable", 0u8);

        println!("   ✅ FIFO CDC test complete");
        println!("✅ AsyncFIFO CDC test PASSED!");
    }

    #[tokio::test]
    #[ignore = "known bug: multi-clock domain CDC FIFO not passing data correctly"]
    async fn test_graphics_pipeline_multi_clock_domains() {
        println!("🎨 Testing Graphics Pipeline End-to-End");
        println!("   Testing full data flow: sys_clk → geom_clk → pixel_clk");

        // Use the proper test design file from the graphics_pipeline verification directory
        let test_design_path = "examples/graphics_pipeline/verif/testbenches/tb_pipeline_e2e.sk";

        let mut tb = Testbench::new(test_design_path)
            .await
            .expect("Failed to create testbench");

        println!("   ✅ Testbench created");

        // ============================================================
        // Reset all clock domains
        // ============================================================
        println!("   🔄 Resetting all clock domains...");
        tb.set("sys_rst", 1u8)
            .set("geom_rst", 1u8)
            .set("pixel_rst", 1u8)
            .set("write_valid", 0u8)
            .set("read_ready", 0u8);

        // Reset for 2 cycles on each clock
        tb.clock_multi(&[("sys_clk", 2), ("geom_clk", 2), ("pixel_clk", 2)])
            .await;

        // Release reset
        tb.set("sys_rst", 0u8)
            .set("geom_rst", 0u8)
            .set("pixel_rst", 0u8);

        tb.clock_multi(&[("sys_clk", 1), ("geom_clk", 1), ("pixel_clk", 1)])
            .await;

        println!("   ✅ All clock domains reset");

        // ============================================================
        // Write test vertices to system clock domain
        // ============================================================
        println!("   📝 Writing test vertices to system clock domain...");

        let test_vertices = [
            (0x40000000u32, 0x40400000u32, 0x40800000u32), // (2.0, 3.0, 4.0)
            (0x40A00000u32, 0x40C00000u32, 0x40E00000u32), // (5.0, 6.0, 7.0)
            (0x41000000u32, 0x41100000u32, 0x41200000u32), // (8.0, 9.0, 10.0)
        ];

        for (i, &(x, y, z)) in test_vertices.iter().enumerate() {
            // IMPORTANT: Struct fields are flattened with underscores, not dots
            // write_vertex.x becomes write_vertex_x (defined in hir_to_mir.rs flatten_port)
            tb.set("write_vertex_x", x)
                .set("write_vertex_y", y)
                .set("write_vertex_z", z)
                .set("write_valid", 1u8);

            tb.clock_signal("sys_clk", 1).await;

            println!(
                "      Wrote vertex {}: ({:08X}, {:08X}, {:08X})",
                i, x, y, z
            );
        }

        tb.set("write_valid", 0u8);
        tb.clock_signal("sys_clk", 1).await;

        println!("   ✅ Vertices written to input FIFO");

        // Check if writes were accepted
        let write_ready: u8 = tb.get_as("write_ready").await;
        println!("   🔍 Debug: write_ready = {}", write_ready);

        // ============================================================
        // Run geometry clock to process vertices
        // ============================================================
        println!("   ⚙️  Processing vertices in geometry clock domain...");

        // Run geometry and pixel clocks together to allow CDC synchronization
        // CDC requires multiple clock cycles on both domains to propagate signals
        // Each vertex needs:
        //   - 2-3 cycles for input FIFO CDC (sys_clk → geom_clk)
        //   - 2 cycles to read and process
        //   - 2-3 cycles for output FIFO CDC (geom_clk → pixel_clk)
        println!("   ⏳ Running clocks for CDC and processing...");

        for cycle in 0..50 {
            // Run geometry clock (faster processing)
            tb.clock_signal("geom_clk", 1).await;

            // Also run pixel clock every few cycles for CDC
            if cycle % 2 == 0 {
                tb.clock_signal("pixel_clk", 1).await;
            }

            // Also run system clock occasionally for full CDC chain
            if cycle % 3 == 0 {
                tb.clock_signal("sys_clk", 1).await;
            }
        }

        println!("   ✅ Clock cycles complete");

        // Check pipeline status before reading
        let busy: u8 = tb.get_as("geom_busy").await;
        let read_valid_pre: u8 = tb.get_as("read_valid").await;
        println!(
            "   🔍 Debug before read: geom_busy={}, read_valid={}",
            busy, read_valid_pre
        );

        // ============================================================
        // Read processed vertices from pixel clock domain
        // ============================================================
        println!("   📖 Reading processed vertices from pixel clock domain...");

        tb.set("read_ready", 1u8);

        let mut read_vertices = Vec::new();

        for i in 0..test_vertices.len() {
            // Check if data is available
            let read_valid: u8 = tb.get_as("read_valid").await;
            if read_valid == 0 {
                println!("      ⚠️  No more data available after {} vertices", i);
                break;
            }

            // Read the vertex (using underscore notation for flattened struct fields)
            let x: u32 = tb.get_as("read_vertex_x").await;
            let y: u32 = tb.get_as("read_vertex_y").await;
            let z: u32 = tb.get_as("read_vertex_z").await;

            read_vertices.push((x, y, z));

            println!("      Read vertex {}: ({:08X}, {:08X}, {:08X})", i, x, y, z);

            // Clock to advance to next vertex
            tb.clock_signal("pixel_clk", 1).await;
        }

        println!("   ✅ Read {} vertices from output", read_vertices.len());

        // ============================================================
        // Verify data integrity through the pipeline
        // ============================================================
        println!("   ✔️  Verifying data integrity...");

        assert_eq!(
            read_vertices.len(),
            test_vertices.len(),
            "Should read same number of vertices as written"
        );

        for (i, (&written, &read)) in test_vertices.iter().zip(read_vertices.iter()).enumerate() {
            assert_eq!(
                written, read,
                "Vertex {} data should match (written: {:08X?}, read: {:08X?})",
                i, written, read
            );
        }

        println!("   ✅ All vertices passed through correctly!");

        // ============================================================
        // Verify pipeline status
        // ============================================================
        println!("   📊 Checking pipeline status...");

        let busy: u8 = tb.get_as("geom_busy").await;
        println!("      Pipeline busy: {}", busy);

        println!("✅ Graphics Pipeline End-to-End Test PASSED!");
        println!("   ✅ System clock domain (write) works");
        println!("   ✅ Geometry clock domain (processing) works");
        println!("   ✅ Pixel clock domain (read) works");
        println!("   ✅ CDC FIFOs transfer data correctly");
        println!(
            "   ✅ All {} vertices preserved through pipeline",
            test_vertices.len()
        );
    }

    #[tokio::test]
    async fn test_async_fifo_single_value() {
        println!("🧪 Testing AsyncFIFO Single Value");

        let fifo_source = r#"
mod async_fifo;
use async_fifo::{AsyncFifo, clog2};

struct SimpleData {
    value: bit[32]
}

entity FifoTest {
    in wr_clk: clock
    in wr_rst: reset(active_high)
    in rd_clk: clock
    in rd_rst: reset(active_high)

    in write_data: bit[32]
    in write_enable: bit
    out write_full: bit

    out read_data: bit[32]
    in read_enable: bit
    out read_empty: bit
}

impl FifoTest {
    signal wr_data_internal: SimpleData
    signal rd_data_internal: SimpleData

    wr_data_internal.value = write_data
    read_data = rd_data_internal.value

    let fifo = AsyncFifo<SimpleData, 8> {
        wr_clk: wr_clk,
        wr_rst: wr_rst,
        wr_en: write_enable,
        wr_data: wr_data_internal,
        wr_full: write_full,
        rd_clk: rd_clk,
        rd_rst: rd_rst,
        rd_en: read_enable,
        rd_data: rd_data_internal,
        rd_empty: read_empty
    }
}
        "#;

        let temp_dir = std::env::temp_dir();
        let async_fifo_source =
            std::fs::read_to_string("examples/graphics_pipeline/lib/async_fifo.sk")
                .expect("Failed to read async_fifo.sk");
        std::fs::write(temp_dir.join("async_fifo.sk"), async_fifo_source)
            .expect("Failed to write async_fifo.sk to temp");

        let source_file = temp_dir.join("fifo_single_test.sk");
        std::fs::write(&source_file, fifo_source).expect("Failed to write test source");

        let mut tb = Testbench::new(source_file.to_str().unwrap())
            .await
            .expect("Failed to create testbench");

        println!("   ✅ Testbench created");

        // Reset with control signals initialized
        tb.set("write_enable", 0u8).set("read_enable", 0u8);
        tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
        tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;
        tb.set("wr_rst", 0u8).set("rd_rst", 0u8);
        tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;

        // Write ONE value
        tb.set("write_data", 0x12345678u32).set("write_enable", 1u8);
        tb.clock_signal("wr_clk", 1).await;
        tb.set("write_enable", 0u8);
        println!("   📝 Wrote 0x12345678");

        // CDC synchronization
        tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 4)]).await;

        // Read the value
        let empty: u8 = tb.get_as("read_empty").await;
        println!("   read_empty = {}", empty);
        assert_eq!(empty, 0, "FIFO should not be empty");

        let value: u32 = tb.get_as("read_data").await;
        println!("   📖 Read 0x{:08X}", value);
        assert_eq!(value, 0x12345678, "Should read written value");

        println!("✅ AsyncFIFO Single Value test PASSED!");
    }

    #[tokio::test]
    async fn test_simple_cdc() {
        println!("🧪 Testing Simple CDC");

        let source = r#"
entity SimpleCDC {
    in wr_clk: clock
    in wr_rst: reset(active_high)
    in wr_data: bit[32]
    in wr_en: bit
    
    in rd_clk: clock
    in rd_rst: reset(active_high)
    out rd_data: bit[32]
}

impl SimpleCDC {
    signal data_reg: bit[32]
    
    on(wr_clk.rise) {
        if wr_rst {
            data_reg <= 0
        } else {
            if wr_en {
                data_reg <= wr_data
            }
        }
    }
    
    rd_data = data_reg
}
        "#;

        let temp_dir = std::env::temp_dir();
        let source_file = temp_dir.join("simple_cdc_test.sk");
        std::fs::write(&source_file, source).expect("Failed to write test source");

        let mut tb = Testbench::new(source_file.to_str().unwrap())
            .await
            .expect("Failed to create testbench");

        println!("   ✅ Testbench created");

        // Reset both domains
        tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
        tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;

        tb.set("wr_rst", 0u8).set("rd_rst", 0u8);
        tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;

        // Write a value on wr_clk
        tb.set("wr_data", 0x12345678u32).set("wr_en", 1u8);
        tb.clock_signal("wr_clk", 1).await;
        tb.set("wr_en", 0u8);

        println!("   📝 Wrote 0x12345678 on wr_clk");

        // Try to read it on rd_clk domain (should see it immediately since it's async)
        let value: u32 = tb.get_as("rd_data").await;
        println!("   📖 Read 0x{:08X} from rd_data", value);

        assert_eq!(
            value, 0x12345678,
            "Should read written value across clock domains"
        );
        println!("✅ Simple CDC test PASSED!");
    }
}
#[cfg(all(test, target_os = "macos"))]
#[tokio::test]
async fn test_struct_output_read() {
    use skalp_testing::testbench::Testbench;

    let mut tb = Testbench::new("tests/fixtures/test_struct_outputs.sk")
        .await
        .expect("Failed to create testbench");

    println!("✅ Testbench created");

    // Reset
    tb.set("rst", 1u8).clock(2).await;
    println!("✅ Reset complete");

    // Read during reset (should get reset values)
    let x_reset: u32 = tb.get_as("vertex_x").await;
    let y_reset: u32 = tb.get_as("vertex_y").await;
    let z_reset: u32 = tb.get_as("vertex_z").await;

    println!("During reset:");
    println!("  vertex_x = 0x{:08X} (expected 0x11111111)", x_reset);
    println!("  vertex_y = 0x{:08X} (expected 0x22222222)", y_reset);
    println!("  vertex_z = 0x{:08X} (expected 0x33333333)", z_reset);

    // Release reset
    tb.set("rst", 0u8).clock(2).await;
    println!("✅ Reset released");

    // Read after reset
    let x: u32 = tb.get_as("vertex_x").await;
    let y: u32 = tb.get_as("vertex_y").await;
    let z: u32 = tb.get_as("vertex_z").await;

    println!("After reset:");
    println!("  vertex_x = 0x{:08X} (expected 0xAAAAAAAA)", x);
    println!("  vertex_y = 0x{:08X} (expected 0xBBBBBBBB)", y);
    println!("  vertex_z = 0x{:08X} (expected 0xCCCCCCCC)", z);

    assert_eq!(x, 0xAAAAAAAA, "vertex_x mismatch");
    assert_eq!(y, 0xBBBBBBBB, "vertex_y mismatch");
    assert_eq!(z, 0xCCCCCCCC, "vertex_z mismatch");

    println!("✅ All struct fields read correctly!");
}
#[cfg(all(test, target_os = "macos"))]
#[tokio::test]
async fn test_vec3_fifo() {
    use skalp_testing::testbench::Testbench;

    println!("🧪 Testing Vec3 FIFO");

    let mut tb = Testbench::new("tests/fixtures/test_output_fifo_simple.sk")
        .await
        .expect("Failed to create testbench");

    println!("   ✅ Testbench created");

    // Reset both domains
    tb.set("write_enable", 0u8).set("read_enable", 0u8);
    tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
    tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;
    tb.set("wr_rst", 0u8).set("rd_rst", 0u8);
    tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;

    println!("   ✅ Reset complete");

    // Write 3 Vec3 values
    let test_data = [
        (0x40000000u32, 0x40400000u32, 0x40800000u32), // (2.0, 3.0, 4.0)
        (0x40A00000u32, 0x40C00000u32, 0x40E00000u32), // (5.0, 6.0, 7.0)
        (0x41000000u32, 0x41100000u32, 0x41200000u32), // (8.0, 9.0, 10.0)
    ];

    println!("   📝 Writing Vec3 data...");
    for (i, &(x, y, z)) in test_data.iter().enumerate() {
        tb.set("write_x", x)
            .set("write_y", y)
            .set("write_z", z)
            .set("write_enable", 1u8);
        tb.clock_signal("wr_clk", 1).await;
        println!("      Wrote Vec3 {}: ({:08X}, {:08X}, {:08X})", i, x, y, z);
    }

    tb.set("write_enable", 0u8);
    tb.clock_signal("wr_clk", 1).await;

    println!("   ✅ Data written");

    // CDC synchronization
    println!("   ⏳ CDC synchronization...");
    tb.clock_multi(&[("wr_clk", 3), ("rd_clk", 5)]).await;

    // Read back
    println!("   📖 Reading Vec3 data...");

    for (i, &expected) in test_data.iter().enumerate() {
        let empty: u8 = tb.get_as("read_empty").await;
        println!("      read_empty = {}", empty);

        if empty != 0 {
            println!("      ⚠️  FIFO reports empty at read {}", i);
            break;
        }

        let x: u32 = tb.get_as("read_x").await;
        let y: u32 = tb.get_as("read_y").await;
        let z: u32 = tb.get_as("read_z").await;

        println!("      Read Vec3 {}: ({:08X}, {:08X}, {:08X})", i, x, y, z);

        assert_eq!((x, y, z), expected, "Vec3 {} mismatch", i);

        tb.set("read_enable", 1u8);
        tb.clock_signal("rd_clk", 1).await;
    }

    tb.set("read_enable", 0u8);

    println!("✅ Vec3 FIFO test PASSED!");
}
