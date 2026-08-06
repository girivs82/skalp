// Regression: `bins low = {[0:127]};` inside a coverpoint body made
// parse_bins_values loop forever — parse_expression consumed no tokens on
// `[` and the set-of-values loop had no progress guard, so the parser hung
// instead of erroring. Parsing must always terminate.

#[cfg(test)]
mod covergroup_parse_termination {
    use skalp_frontend::parse_and_build_hir;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    /// Run the parser on a worker thread and fail if it doesn't return.
    fn assert_parse_terminates(source: &'static str) {
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            // The result (Ok or Err) is irrelevant — only termination is.
            let _ = tx.send(parse_and_build_hir(source).is_ok());
        });
        rx.recv_timeout(Duration::from_secs(30))
            .expect("parser hung (did not terminate within 30s)");
    }

    #[test]
    fn covergroup_with_range_bins_terminates() {
        assert_parse_terminates(
            r#"
            entity Counter {
                in clk: clock
                out count: bit[8]
            }

            impl Counter {
                signal counter: bit[8] = 0

                on(clk.rise) {
                    counter = counter + 1
                }

                covergroup cg @(posedge clk) {
                    coverpoint counter {
                        bins low = {[0:127]};
                        bins high = {[128:255]};
                    }
                }

                count = counter
            }
            "#,
        );
    }

    #[test]
    fn systemverilog_style_covergroup_errors_fast() {
        // SV-style `;` body + endgroup is not SKALP syntax — it must produce
        // parse errors, not a hang.
        assert_parse_terminates(
            r#"
            entity Counter {
                in clk: clock
                out count: bit[8]
            }

            impl Counter {
                signal counter: bit[8] = 0

                covergroup cg @(posedge clk);
                    coverpoint counter {
                        bins low = {[0:127]};
                    }
                endgroup

                count = counter
            }
            "#,
        );
    }
}
