// Power-domain declarations, the dependent-failure (CCF) check, and UPF
// emission (spec power-domain subset).

#[cfg(test)]
mod power_domains {
    use skalp_frontend::hir::HirPowerDerivation;
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::lower_to_mir;

    /// The CHECKED pipeline (the one `skalp build` runs). `lower_to_mir`
    /// is the raw transform and skips the compiler's silently-wrong-hardware
    /// checks, including the power-domain CCF check.
    fn compile_checked(hir: &skalp_frontend::hir::Hir) -> Result<skalp_mir::Mir, String> {
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(hir)
    }

    const SUPPLY_TREE: &str = r#"
        power_domain vreg_main: external;
        power_domain vdd_core = regulated(vreg_main, macro = u_ldo_core, states = { on: 0.9V, ret: 0.6V, off });
        power_domain vdd_periph = regulated(vreg_main, macro = u_ldo_p, states = { on: 1.8V });
        power_domain vdd_mon: external;
    "#;

    const WATCHDOG: &str = r#"
        #[safety_mechanism(type = watchdog)]
        entity Watchdog {
            in clk: clock
            in kick: bit
            out timeout: bit
        }

        impl Watchdog {
            signal cnt: bit[8] = 0
            on(clk.rise) {
                if kick {
                    cnt = 0
                } else {
                    cnt = cnt + 1
                }
            }
            timeout = cnt == 255
        }

        #[power_domain(vdd_core)]
        entity Controller {
            in clk: clock
            in kick: bit
            out wd_timeout: bit
            out q: bit[8]
        }

        impl Controller {
            inst wd = Watchdog { clk: clk, kick: kick }
            signal c: bit[8] = 0
            on(clk.rise) { c = c + 1 }
            q = c
            wd_timeout = wd.timeout
        }
    "#;

    fn design(mechanism_attr: &str) -> String {
        format!(
            "{}\n{}",
            SUPPLY_TREE,
            WATCHDOG.replacen(
                "#[safety_mechanism(type = watchdog)]",
                &format!(
                    "{}\n        #[safety_mechanism(type = watchdog)]",
                    mechanism_attr
                ),
                1
            )
        )
    }

    #[test]
    fn declarations_build_the_supply_tree() {
        let hir = parse_and_build_hir(SUPPLY_TREE).expect("parse");
        assert_eq!(hir.power_domain_decls.len(), 4);
        let core = &hir.power_domain_decls[1];
        assert_eq!(core.name, "vdd_core");
        match &core.derivation {
            HirPowerDerivation::Regulated { parent, macro_name } => {
                assert_eq!(parent, "vreg_main");
                assert_eq!(macro_name.as_deref(), Some("u_ldo_core"));
            }
            other => panic!("wrong derivation: {other:?}"),
        }
        assert_eq!(core.states.len(), 3);
        assert_eq!(core.states[0].voltage_mv, Some(900));
        assert_eq!(core.states[2].voltage_mv, None); // off
    }

    #[test]
    fn switched_domain_records_polarity_as_expression() {
        let src = r#"
            power_domain vdd_core: external;
            power_domain vdd_gpu = switched(vdd_core, on_when = !pmu.gpu_sleep, ack_on = pmu.gpu_ack);
        "#;
        let hir = parse_and_build_hir(src).expect("parse");
        match &hir.power_domain_decls[1].derivation {
            HirPowerDerivation::Switched {
                parent,
                on_when,
                ack_on,
            } => {
                assert_eq!(parent, "vdd_core");
                let on = on_when.as_ref().expect("on_when");
                assert!(
                    on.inverted,
                    "polarity must come from the ! in the expression"
                );
                assert_eq!(on.path, ["pmu", "gpu_sleep"]);
                assert!(!ack_on.as_ref().unwrap().inverted);
            }
            other => panic!("wrong derivation: {other:?}"),
        }
    }

    #[test]
    fn undeclared_reference_and_cycle_are_errors() {
        let err = parse_and_build_hir(
            r#"
            power_domain a = regulated(b);
            power_domain b = switched(a);
            #[power_domain(nope)]
            entity E { in x: bit out y: bit }
            impl E { y = x }
            "#,
        )
        .expect_err("must fail");
        let msg = format!("{err:#}");
        assert!(msg.contains("cycle"), "missing cycle error: {msg}");
        assert!(
            msg.contains("undeclared power domain"),
            "missing ref error: {msg}"
        );
    }

    #[test]
    fn shared_supply_mechanism_fails_the_build() {
        let hir = parse_and_build_hir(&design("#[power_domain(vdd_periph)]")).expect("parse");
        let err = compile_checked(&hir).expect_err("shared-regulator watchdog must fail CCF");
        assert!(
            err.contains("common-cause failure") && err.contains("vdd_periph"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn independent_supply_mechanism_passes() {
        let hir = parse_and_build_hir(&design("#[power_domain(vdd_mon)]")).expect("parse");
        compile_checked(&hir).expect("independent-supply watchdog must pass CCF");
    }

    #[test]
    fn allow_shared_supply_downgrades_to_warning() {
        let hir = parse_and_build_hir(&design("#[power_domain(vdd_periph, allow_shared_supply)]"))
            .expect("parse");
        compile_checked(&hir).expect("justified shared supply must build");
    }

    #[test]
    fn upf_emission_covers_tree_switch_and_states() {
        let src = r#"
            power_domain vreg_main: external;
            power_domain vdd_core = regulated(vreg_main, macro = u_ldo, states = { on: 0.9V, off });
            power_domain vdd_gpu = switched(vdd_core, on_when = !pmu.gpu_sleep, ack_on = pmu.gpu_ack, states = { on: 0.9V, off });

            #[power_domain(vdd_core)]
            entity Top {
                in clk: clock
                out q: bit[8]
            }

            impl Top {
                signal c: bit[8] = 0
                on(clk.rise) { c = c + 1 }
                q = c
            }
        "#;
        let hir = parse_and_build_hir(src).expect("parse");
        let mir = lower_to_mir(&hir).expect("mir");
        let upf = skalp_mir::upf::generate_upf(&hir, &mir, "Top").expect("upf");

        for needle in [
            "upf_version 2.1",
            "create_supply_port VREG_MAIN",
            "create_power_domain PD_vdd_core -elements {.}",
            "create_power_switch SW_vdd_gpu",
            "-on_state {on_s sw_in {!pmu/gpu_sleep}}",
            "-ack_port {sw_ack pmu/gpu_ack {pmu/gpu_ack}}",
            "add_power_state SS_vdd_gpu -state off",
        ] {
            assert!(upf.contains(needle), "UPF missing `{needle}`:\n{upf}");
        }
    }

    #[test]
    fn no_declarations_means_no_upf_and_no_checks() {
        // Legacy annotation-only designs are untouched.
        let src = r#"
            #[power_domain("vdd_free_string")]
            entity E {
                in clk: clock
                out q: bit
            }
            impl E {
                signal s: bit = 0
                on(clk.rise) { s = !s }
                q = s
            }
        "#;
        let hir = parse_and_build_hir(src).expect("legacy string form still parses");
        let mir = lower_to_mir(&hir).expect("no checks without declarations");
        assert!(skalp_mir::upf::generate_upf(&hir, &mir, "E").is_none());
    }
}

#[cfg(test)]
mod instance_output_vs_trait_inline {
    use skalp_frontend::parse_and_build_compilation_context;
    use std::io::Write;

    /// Regression: with the stdlib loaded, inlining `c + 1` (impl Add ->
    /// std_adder) registered the trait body's `let adder` under VariableId(0),
    /// clobbering the user impl's `inst wd` entry (also id 0) mid-impl — a
    /// later `wd.timeout` resolved to the ADDER's ports and the assignment
    /// conversion panicked (Bug #85 guard). Instance maps are now
    /// snapshot/restored around trait-method inlining.
    #[test]
    fn instance_output_read_plus_arithmetic_with_stdlib() {
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");
        let source = r#"
        entity Watchdog {
            in clk: clock
            in kick: bit
            out timeout: bit
        }

        impl Watchdog {
            signal cnt: bit[8] = 0
            on(clk.rise) {
                if kick {
                    cnt = 0
                } else {
                    cnt = cnt + 1
                }
            }
            timeout = cnt == 255
        }

        entity Controller {
            in clk: clock
            in kick: bit
            out wd_timeout: bit
            out q: bit[8]
        }

        impl Controller {
            inst wd = Watchdog { clk: clk, kick: kick }
            signal c: bit[8] = 0
            on(clk.rise) { c = c + 1 }
            q = c
            wd_timeout = wd.timeout
        }
        "#;
        let temp =
            std::env::temp_dir().join(format!("inst_out_trait_inline_{}.sk", std::process::id()));
        let mut f = std::fs::File::create(&temp).expect("temp file");
        f.write_all(source.as_bytes()).expect("write");
        let ctx = parse_and_build_compilation_context(&temp).expect("parse + modules");
        let _ = std::fs::remove_file(&temp);

        let mir = skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
            .expect("instance-output read + arithmetic must compile with stdlib loaded");
        let ctrl = mir
            .modules
            .iter()
            .find(|m| m.name == "Controller")
            .expect("Controller");
        assert!(
            ctrl.assignments.len() >= 2,
            "both q and wd_timeout must be driven"
        );
    }
}

#[cfg(test)]
mod fpga_leg {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::fpga_power::fpga_power_posture;

    fn compile_checked(src: &str) -> Result<skalp_mir::Mir, String> {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
    }

    fn blink(clk_std: &str) -> String {
        format!(
            r#"
            constraint physical {{
                bank 0 {{ voltage: 1.8, io_standard: "LVCMOS18" }}
                bank 1 {{ voltage: 3.3, io_standard: "LVCMOS33" }}
            }}

            entity Blink {{
                in clk: clock @ {{ pin: "A1", io_standard: "{clk_std}", bank: 0 }}
                out led: bit @ {{ pin: "B2", io_standard: "LVCMOS33", bank: 1 }}
            }}

            impl Blink {{
                signal c: bit = 0
                on(clk.rise) {{ c = !c }}
                led = c
            }}
            "#
        )
    }

    #[test]
    fn io_standard_vs_bank_rail_mismatch_fails() {
        let err = compile_checked(&blink("LVCMOS33")).expect_err("3.3V pin on 1.8V bank");
        assert!(
            err.contains("VCCIO mismatch") && err.contains("bank 0"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn matching_bank_rails_pass() {
        compile_checked(&blink("LVCMOS18")).expect("matching rails must build");
    }

    #[test]
    fn unknown_io_standard_is_not_an_error() {
        compile_checked(&blink("EXOTIC_IO")).expect("unknown standards are skipped");
    }

    #[test]
    fn float_constraint_values_parse_and_terminate() {
        // Regression: `voltage: 1.8` (FloatLiteral) had no arm in
        // parse_constraint_value and the error path consumed nothing —
        // the bank-block loop hung forever.
        compile_checked(&blink("LVCMOS18")).expect("float bank voltages parse");
    }

    #[test]
    fn stub_posture_decisions() {
        let src = r#"
            power_domain vreg: external;
            power_domain vdd_core = regulated(vreg, macro = u_ldo, states = { on: 0.9V });
            power_domain vdd_gpu = switched(vdd_core, on_when = !slp);

            entity E { in x: bit out y: bit }
            impl E { y = x }
        "#;
        let hir = parse_and_build_hir(src).expect("parse");

        let err = fpga_power_posture(&hir, false).expect_err("must refuse without stub");
        assert!(err.contains("--power-stub") && err.contains("vdd_gpu"));

        let report = fpga_power_posture(&hir, true)
            .expect("stub allowed")
            .expect("must produce a report");
        assert!(report.contains("STUBBED") && report.contains("VCCINT"));

        // External-only trees need no stubbing at all.
        let ext_only = parse_and_build_hir(
            "power_domain vddio: external;\nentity E { in x: bit out y: bit }\nimpl E { y = x }",
        )
        .expect("parse");
        assert!(fpga_power_posture(&ext_only, false).expect("ok").is_none());
    }
}

#[cfg(test)]
mod control_cone {
    use skalp_frontend::parse_and_build_hir;

    fn compile_checked(src: &str) -> Result<skalp_mir::Mir, String> {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
    }

    fn design(pmu_domain: &str) -> String {
        format!(
            r#"
            power_domain vdd_aon: external, states = {{ on: 0.9V }};
            power_domain vreg: external;
            power_domain vdd_core = regulated(vreg, macro = u_ldo, states = {{ on: 0.9V, off }});
            power_domain vdd_gpu = switched(vdd_core, on_when = !pmu.gpu_sleep, states = {{ on: 0.9V, off }});

            #[power_domain({pmu_domain})]
            entity Pmu {{
                in clk: clock
                out gpu_sleep: bit
            }}

            impl Pmu {{
                signal s: bit = 0
                on(clk.rise) {{ s = !s }}
                gpu_sleep = s
            }}

            entity Top {{
                in clk: clock
                out slp: bit
            }}

            impl Top {{
                inst pmu = Pmu {{ clk: clk }}
                slp = pmu.gpu_sleep
            }}
            "#
        )
    }

    #[test]
    fn always_on_controller_passes() {
        compile_checked(&design("vdd_aon")).expect("AON controller must pass");
    }

    #[test]
    fn self_powered_control_is_an_error() {
        // The PMU inside the domain it switches: a domain cannot switch its
        // own supply back on.
        let err = compile_checked(&design("vdd_gpu")).expect_err("no-self-power must fail");
        assert!(
            err.contains("no-self-power") && err.contains("vdd_gpu"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn switchable_controller_warns_but_builds() {
        // vdd_core can power off (declares an `off` state): liveness warning
        // only — the build succeeds.
        compile_checked(&design("vdd_core")).expect("liveness is a warning, not an error");
    }
}
