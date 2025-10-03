#!/usr/bin/env python3
"""
Simple LSP client to test the SKALP LSP server
"""

import json
import subprocess
import sys

def send_lsp_message(process, message):
    """Send a properly formatted LSP message"""
    content = json.dumps(message)
    header = f"Content-Length: {len(content)}\r\n\r\n"
    full_message = header + content

    print(f"Sending: {full_message}", file=sys.stderr)

    process.stdin.write(full_message.encode())
    process.stdin.flush()

def read_lsp_response(process):
    """Read an LSP response"""
    # Read headers
    headers = {}
    while True:
        line = process.stdout.readline().decode().strip()
        if not line:
            break
        if ':' in line:
            key, value = line.split(':', 1)
            headers[key.strip()] = value.strip()

    # Read content
    content_length = int(headers.get('Content-Length', 0))
    if content_length > 0:
        content = process.stdout.read(content_length).decode()
        return json.loads(content)
    return None

def test_lsp_server():
    """Test the LSP server with a basic initialize request"""

    # Start the LSP server
    process = subprocess.Popen(
        ['./target/release/skalp-lsp'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )

    try:
        # Send initialize request
        initialize_request = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "processId": None,
                "rootUri": "file:///Users/girivs/src/hw/hls",
                "capabilities": {
                    "textDocument": {
                        "hover": {
                            "contentFormat": ["markdown"]
                        }
                    }
                }
            }
        }

        send_lsp_message(process, initialize_request)

        # Read response
        response = read_lsp_response(process)
        print(f"Response: {response}")

        if response and response.get('id') == 1:
            print("✅ LSP server responded correctly")

            # Send initialized notification
            initialized_notification = {
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }

            send_lsp_message(process, initialized_notification)
            print("✅ Sent initialized notification")

        else:
            print("❌ LSP server did not respond correctly")

    except Exception as e:
        print(f"❌ Error: {e}")

    finally:
        # Read any stderr output
        try:
            stderr_output = process.stderr.read(1024).decode()
            if stderr_output:
                print(f"Stderr: {stderr_output}")
        except:
            pass

        process.terminate()
        process.wait()

if __name__ == "__main__":
    test_lsp_server()