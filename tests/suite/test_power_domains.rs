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

#[cfg(test)]
mod power_state_table {
    use skalp_frontend::parse_and_build_hir;

    fn compile_checked(src: &str) -> Result<skalp_mir::Mir, String> {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
    }

    fn design(sleep_state: &str, pmu_domain: &str) -> String {
        format!(
            r#"
            power_domain vdd_aon: external, states = {{ on: 0.9V }};
            power_domain vreg: external;
            power_domain vdd_core = regulated(vreg, macro = u_ldo, states = {{ on: 0.9V, ret: 0.6V, off }});
            power_domain vdd_gpu = switched(vdd_core, on_when = !pmu.gpu_sleep, states = {{ on: 0.9V, off }});

            power_states {{
                run   = {{ vdd_aon: on, vdd_core: on, vdd_gpu: on }},
                sleep = {{ {sleep_state} }},
            }};

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
    fn valid_pst_builds_and_exports() {
        let src = design("vdd_aon: on, vdd_core: ret, vdd_gpu: off", "vdd_aon");
        let hir = parse_and_build_hir(&src).expect("parse");
        let mir = compile_checked(&src).expect("valid PST must build");
        let upf = skalp_mir::upf::generate_upf(&hir, &mir, "Top").expect("upf");
        assert!(upf.contains("create_pst SYSTEM_PST"));
        assert!(upf.contains("add_pst_state sleep -pst SYSTEM_PST -state {on ret off}"));
    }

    #[test]
    fn powered_child_of_off_parent_is_illegal() {
        let err = compile_checked(&design(
            "vdd_aon: on, vdd_core: off, vdd_gpu: on",
            "vdd_aon",
        ))
        .expect_err("gpu on while core off must fail");
        assert!(
            err.contains("cannot be up while its source is down"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn pst_liveness_is_precise_and_fatal() {
        // With a PST declared, the controller check is per-state and an
        // ERROR: the PMU in vdd_core, which is `ret` in sleep, fails.
        let err = compile_checked(&design(
            "vdd_aon: on, vdd_core: ret, vdd_gpu: off",
            "vdd_core",
        ))
        .expect_err("controller not on in sleep must fail");
        assert!(
            err.contains("PST-liveness") && err.contains("sleep"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn unknown_state_and_incompleteness_are_errors() {
        let err = parse_and_build_hir(&design("vdd_core: retention", "vdd_aon"))
            .map(|_| ())
            .expect_err("must fail at HIR validation");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("has no state `retention`"),
            "missing ref error: {msg}"
        );
        assert!(
            msg.contains("must appear in every system state"),
            "missing completeness error: {msg}"
        );
    }
}

#[cfg(test)]
mod bank_domain_and_deep_paths {
    use skalp_frontend::parse_and_build_hir;

    fn compile_checked(src: &str) -> Result<skalp_mir::Mir, String> {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
    }

    fn design(io_std: &str, pmu_domain: &str) -> String {
        format!(
            r#"
            power_domain vdd_aon: external, states = {{ on: 0.9V }};
            power_domain vddio_a: external, states = {{ on: 3.3V }};
            power_domain vreg: external;
            power_domain vdd_core = regulated(vreg, macro = u_ldo, states = {{ on: 0.9V, off }});
            power_domain vdd_gpu = switched(vdd_core, on_when = !soc.pmu.gpu_sleep, states = {{ on: 0.9V, off }});

            constraint physical {{
                bank 0 {{ domain: vddio_a }}
            }}

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

            entity Soc {{
                in clk: clock
                out slp: bit
            }}

            impl Soc {{
                inst pmu = Pmu {{ clk: clk }}
                slp = pmu.gpu_sleep
            }}

            entity Top {{
                in clk: clock @ {{ pin: "A1", io_standard: "{io_std}", bank: 0 }}
                out slp: bit
            }}

            impl Top {{
                inst soc = Soc {{ clk: clk }}
                slp = soc.slp
            }}
            "#
        )
    }

    #[test]
    fn domain_fed_bank_and_deep_path_pass() {
        compile_checked(&design("LVCMOS33", "vdd_aon")).expect("valid design must build");
    }

    #[test]
    fn domain_fed_bank_drives_the_vccio_check() {
        // Bank 0's rail comes from vddio_a's `on` state (3.3V), no literal.
        let err = compile_checked(&design("LVCMOS18", "vdd_aon"))
            .expect_err("1.8V pin on domain-fed 3.3V bank");
        assert!(err.contains("VCCIO mismatch"), "wrong error: {err}");
    }

    #[test]
    fn deep_control_path_resolves_for_no_self_power() {
        // Two hops: Top -> soc -> pmu; PMU bound inside the gated domain.
        let err = compile_checked(&design("LVCMOS33", "vdd_gpu"))
            .expect_err("deep self-powered control must fail");
        assert!(
            err.contains("no-self-power") && err.contains("soc.pmu.gpu_sleep"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn undeclared_bank_domain_is_an_error() {
        let src = design("LVCMOS33", "vdd_aon").replace("domain: vddio_a", "domain: nope");
        let err = compile_checked(&src).expect_err("undeclared bank domain");
        assert!(
            err.contains("undeclared power domain"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn literal_vs_domain_voltage_disagreement_is_an_error() {
        let src = design("LVCMOS33", "vdd_aon")
            .replace("domain: vddio_a", "voltage: 1.8, domain: vddio_a");
        let err = compile_checked(&src).expect_err("1.8 literal vs 3.3 domain");
        assert!(err.contains("disagrees with domain"), "wrong error: {err}");
    }
}

#[cfg(test)]
mod transition_graph {
    use skalp_frontend::parse_and_build_hir;

    fn compile_checked(src: &str) -> Result<skalp_mir::Mir, String> {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
    }

    fn design(transitions: &str) -> String {
        format!(
            r#"
            power_domain vdd_aon: external, states = {{ on: 0.9V }};
            power_domain vreg: external;
            power_domain vdd_core = regulated(vreg, macro = u_ldo, states = {{ on: 0.9V, ret: 0.6V }});
            power_domain vdd_gpu = switched(vdd_core, on_when = !pmu.gpu_sleep, states = {{ on: 0.9V, off }});

            power_states {{
                run   = {{ vdd_aon: on, vdd_core: on,  vdd_gpu: on }},
                idle  = {{ vdd_aon: on, vdd_core: on,  vdd_gpu: off }},
                sleep = {{ vdd_aon: on, vdd_core: ret, vdd_gpu: off }},
                transitions = {{ {transitions} }},
            }};

            #[power_domain(vdd_core)]
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
    fn per_edge_liveness_accepts_what_per_state_rejects() {
        // The PMU lives in vdd_core, which is `ret` in sleep — the
        // conservative per-state rule would reject this. With the graph,
        // vdd_gpu only switches on run<->idle (core on at both ends): PASS.
        let hir = parse_and_build_hir(&design(
            "run -> idle, idle -> run, idle -> sleep, sleep -> idle",
        ))
        .expect("parse");
        assert_eq!(hir.power_states_decl.as_ref().unwrap().transitions.len(), 4);
        compile_checked(&design(
            "run -> idle, idle -> run, idle -> sleep, sleep -> idle",
        ))
        .expect("per-edge analysis must accept the sleepable controller");
    }

    #[test]
    fn switching_edge_with_dead_controller_fails() {
        // A direct run -> sleep edge switches vdd_gpu while the controller
        // domain is `ret` at the sleep endpoint.
        let err = compile_checked(&design(
            "run -> sleep, sleep -> run, run -> idle, idle -> run, idle -> sleep, sleep -> idle",
        ))
        .expect_err("run->sleep switches gpu with controller in ret");
        assert!(
            err.contains("both endpoints") && err.contains("run -> sleep"),
            "wrong error: {err}"
        );
    }

    #[test]
    fn unknown_endpoint_and_self_loop_are_errors() {
        let err = parse_and_build_hir(&design("run -> nowhere, idle -> idle"))
            .map(|_| ())
            .expect_err("must fail validation");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("unknown system power state `nowhere`"),
            "{msg}"
        );
        assert!(msg.contains("self-loops are not meaningful"), "{msg}");
    }

    #[test]
    fn upf_exports_transitions() {
        let src = design("run -> idle, idle -> run, idle -> sleep, sleep -> idle");
        let hir = parse_and_build_hir(&src).expect("parse");
        let mir = compile_checked(&src).expect("mir");
        let upf = skalp_mir::upf::generate_upf(&hir, &mir, "Top").expect("upf");
        assert!(upf.contains("describe_state_transition T0_run_idle -from {run} -to {idle}"));
    }
}

#[cfg(test)]
mod domain_loss_fi {
    use skalp_frontend::parse_and_build_hir;

    /// Full domain-loss FI pipeline: hierarchical synth -> flatten -> SIR ->
    /// domain attribution by net prefix -> kill campaign. The watchdog lives
    /// on vdd_mon and must DETECT the death of vdd_core (its kick path goes
    /// quiet, the timeout fires on the #[detection_signal] output).
    #[test]
    fn watchdog_detects_loss_of_monitored_domain() {
        let src = r#"
        power_domain vreg: external;
        power_domain vdd_core = regulated(vreg, macro = u_ldo, states = { on: 0.9V, off });
        power_domain vdd_mon: external;

        #[power_domain(vdd_mon)]
        #[safety_mechanism(type = watchdog)]
        entity Watchdog {
            in clk: clock
            in kick: bit
            #[detection_signal]
            out timeout: bit
        }

        impl Watchdog {
            signal cnt: bit[4] = 0
            on(clk.rise) {
                if kick {
                    cnt = 0
                } else {
                    if cnt < 8 {
                        cnt = cnt + 1
                    }
                }
            }
            timeout = cnt == 8
        }

        #[power_domain(vdd_core)]
        entity Controller {
            in clk: clock
            out heartbeat: bit
            out wd_timeout: bit
        }

        impl Controller {
            // The kicker: alternates every cycle while alive; dead when
            // vdd_core is killed.
            signal beat: bit = 0
            on(clk.rise) { beat = !beat }
            heartbeat = beat

            inst wd = Watchdog { clk: clk, kick: beat }
            wd_timeout = wd.timeout
        }
        "#;
        let hir = parse_and_build_hir(src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir(&hir)
            .expect("mir");

        let library = skalp_lir::get_stdlib_library("generic_asic").expect("lib");
        let (hier_lir, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
        let hier = skalp_lir::synthesize_hierarchical(
            &hier_lir,
            &library,
            skalp_lir::synth::SynthPreset::Balanced,
        );
        let netlist = hier.flatten();
        let sir_result = skalp_sim::convert_gate_netlist_to_sir(&netlist);

        let prefixes = skalp_mir::fpga_power::domain_instance_prefixes(&hir);
        assert!(
            prefixes.iter().any(|(d, p)| d == "vdd_mon" && p == "wd"),
            "watchdog instance must map to vdd_mon: {prefixes:?}"
        );
        let domains = skalp_sim::domain_primitive_sets(&sir_result.sir, &prefixes);
        let core = domains
            .iter()
            .find(|(d, _)| d == "vdd_core")
            .expect("vdd_core set");
        let mon = domains
            .iter()
            .find(|(d, _)| d == "vdd_mon")
            .expect("vdd_mon set");
        assert!(
            !core.1.is_empty() && !mon.1.is_empty(),
            "both domains must own primitives"
        );

        let mut sim = skalp_sim::GateLevelSimulator::new(&sir_result.sir);
        let results = sim.run_domain_loss_campaign(&domains, 40, "clk");
        let core_result = results
            .iter()
            .find(|r| r.domain == "vdd_core")
            .expect("vdd_core result");
        assert!(
            core_result.detected,
            "watchdog on vdd_mon must DETECT the loss of vdd_core: {core_result:?}"
        );
    }
}

/// Port-granular isolation and level-shifter inference (spec §18.11).
mod port_granular_strategies {
    use skalp_frontend::parse_and_build_hir;

    /// Common design: an always-on rail, a gated rail at the same voltage,
    /// and a separate 1.8 V rail. The gated domain's OUTPUT needs isolation;
    /// its inputs do not (their source cannot go down). The 1.8 V rail needs
    /// shifters in both directions.
    fn soc(extra_iso: bool) -> String {
        let iso = if extra_iso {
            "#[isolation(clamp = low)]\n            "
        } else {
            ""
        };
        format!(
            r#"
        power_domain vbat: external;
        power_domain vdd_aon = regulated(vbat, macro = u_aon, states = {{ on: 0.9V }});
        power_domain vdd_gpu = switched(vdd_aon, on_when = !gpu_sleep, states = {{ on: 0.9V, off }});
        power_domain vdd_io  = regulated(vbat, macro = u_io, states = {{ on: 1.8V }});

        #[power_domain(vdd_gpu)]
        entity Gpu {{
            in clk: clock
            in cmd: bit[8]
            {iso}out result: bit[16]
        }}

        impl Gpu {{
            signal acc: bit[16] = 0
            on(clk.rise) {{ acc = acc + cmd }}
            result = acc
        }}

        #[power_domain(vdd_io)]
        entity Pads {{
            in clk: clock
            in d: bit[8]
            out q: bit[8]
        }}

        impl Pads {{
            signal r: bit[8] = 0
            on(clk.rise) {{ r = d }}
            q = r
        }}

        #[power_domain(vdd_aon)]
        entity Soc {{
            in clk: clock
            in gpu_sleep: bit
            in cmd: bit[8]
            out result: bit[16]
            out io_q: bit[8]
        }}

        impl Soc {{
            inst g = Gpu {{ clk: clk, cmd: cmd }}
            inst p = Pads {{ clk: clk, d: cmd }}
            result = g.result
            io_q = p.q
        }}
        "#
        )
    }

    fn warnings(src: &str) -> Vec<String> {
        let hir = parse_and_build_hir(src).expect("parse");
        let (errors, warnings) = skalp_mir::MirCompiler::check_power_domains(&hir);
        assert!(errors.is_empty(), "design must be CCF-clean: {errors:#?}");
        warnings
    }

    #[test]
    fn output_of_gated_domain_needs_isolation() {
        let w = warnings(&soc(false));
        assert!(
            w.iter()
                .any(|m| m.contains("port `g.result`") && m.contains("no #[isolation] strategy")),
            "gated domain's output must want isolation: {w:#?}"
        );
    }

    #[test]
    fn inputs_from_always_on_source_need_no_isolation() {
        let w = warnings(&soc(false));
        // cmd/clk flow vdd_aon -> vdd_gpu. The SOURCE cannot power off, so
        // there is nothing to clamp — the coarse edge check used to flag these.
        assert!(
            !w.iter()
                .any(|m| m.contains("port `g.cmd`") && m.contains("isolation")),
            "input from an always-on rail must NOT be flagged: {w:#?}"
        );
        assert!(
            !w.iter()
                .any(|m| m.contains("port `g.clk`") && m.contains("isolation")),
            "clock from an always-on rail must NOT be flagged: {w:#?}"
        );
    }

    #[test]
    fn declared_isolation_silences_the_port() {
        let w = warnings(&soc(true));
        assert!(
            !w.iter()
                .any(|m| m.contains("port `g.result`") && m.contains("isolation")),
            "#[isolation] on the port must satisfy the check: {w:#?}"
        );
    }

    #[test]
    fn level_shifters_inferred_from_declared_voltages() {
        let w = warnings(&soc(false));
        assert!(
            w.iter().any(|m| m.contains("port `p.d`")
                && m.contains("level shifter (up)")
                && m.contains("0.90 V")
                && m.contains("1.80 V")),
            "0.9 V -> 1.8 V input needs an up-shifter: {w:#?}"
        );
        assert!(
            w.iter()
                .any(|m| m.contains("port `p.q`") && m.contains("level shifter (down)")),
            "1.8 V -> 0.9 V output needs a down-shifter: {w:#?}"
        );
        assert!(
            !w.iter()
                .any(|m| m.contains("port `g.") && m.contains("level shifter")),
            "equal-voltage crossing needs no shifter: {w:#?}"
        );
    }

    /// With a declared PST, the isolation requirement is precise: a domain
    /// that COULD be gated but is never actually off while its sink is on in
    /// any declared system state needs no isolation.
    #[test]
    fn declared_pst_makes_the_requirement_precise() {
        let base = r#"
        power_domain vbat: external;
        power_domain vdd_aon = regulated(vbat, macro = u_aon, states = { on: 0.9V });
        power_domain vdd_gpu = switched(vdd_aon, on_when = !gpu_sleep, states = { on: 0.9V, off });

        #[power_domain(vdd_gpu)]
        entity Gpu {
            in clk: clock
            in cmd: bit[8]
            out result: bit[16]
        }

        impl Gpu {
            signal acc: bit[16] = 0
            on(clk.rise) { acc = acc + cmd }
            result = acc
        }

        #[power_domain(vdd_aon)]
        entity Soc {
            in clk: clock
            in gpu_sleep: bit
            in cmd: bit[8]
            out result: bit[16]
        }

        impl Soc {
            inst g = Gpu { clk: clk, cmd: cmd }
            result = g.result
        }
        "#;

        // PST that never turns the GPU off: nothing to clamp.
        let never_off = format!(
            "{base}
        power_states {{ run = {{ vdd_aon: on, vdd_gpu: on }} }};"
        );
        let w = warnings(&never_off);
        assert!(
            !w.iter().any(|m| m.contains("isolation")),
            "no declared state has the GPU off — no isolation needed: {w:#?}"
        );

        // PST with a sleep state that does: the clamp is required.
        let sleeps = format!(
            "{base}
        power_states {{ run = {{ vdd_aon: on, vdd_gpu: on }},              sleep = {{ vdd_aon: on, vdd_gpu: off }} }};"
        );
        let w = warnings(&sleeps);
        assert!(
            w.iter()
                .any(|m| m.contains("port `g.result`") && m.contains("isolation")),
            "a declared state has the GPU off while AON is on: {w:#?}"
        );
    }

    /// `#[isolation]` on a PORT must reach the HIR. It used to be dropped
    /// on the floor (`isolation_config: None, // TODO`), so the check could
    /// never be satisfied on the port it names.
    #[test]
    fn port_level_isolation_attribute_reaches_the_hir() {
        let src = r#"
        power_domain vbat: external;
        power_domain vdd_aon = regulated(vbat, macro = u_aon, states = { on: 0.9V });
        power_domain vdd_gpu = switched(vdd_aon, on_when = !gpu_sleep, states = { on: 0.9V, off });

        #[power_domain(vdd_gpu)]
        entity Gpu {
            in clk: clock
            in cmd: bit[8]
            #[isolation(clamp = low)]
            out result: bit[16]
        }

        impl Gpu {
            signal acc: bit[16] = 0
            on(clk.rise) { acc = acc + cmd }
            result = acc
        }

        #[power_domain(vdd_aon)]
        entity Soc {
            in clk: clock
            in gpu_sleep: bit
            in cmd: bit[8]
            out result: bit[16]
        }

        impl Soc {
            inst g = Gpu { clk: clk, cmd: cmd }
            result = g.result
        }
        "#;
        let hir = parse_and_build_hir(src).expect("parse");
        let gpu = hir.entities.iter().find(|e| e.name == "Gpu").expect("Gpu");
        let result = gpu
            .ports
            .iter()
            .find(|p| p.name == "result")
            .expect("result port");
        assert!(
            result.isolation_config.is_some(),
            "#[isolation] on a port must populate isolation_config"
        );
        // ...and the annotation must NOT leak onto neighbouring ports.
        assert!(
            gpu.ports
                .iter()
                .filter(|p| p.name != "result")
                .all(|p| p.isolation_config.is_none()),
            "port isolation must not leak to other ports"
        );
        let w = warnings(src);
        assert!(
            !w.iter()
                .any(|m| m.contains("port `g.result`") && m.contains("isolation")),
            "the annotated port must satisfy the check: {w:#?}"
        );
    }

    /// One annotated signal must not blanket-suppress an entity's OTHER
    /// ports — that would defeat the per-port analysis.
    #[test]
    fn isolation_on_one_signal_does_not_cover_every_port() {
        let src = r#"
        power_domain vbat: external;
        power_domain vdd_mon: external;
        power_domain vdd_sys = regulated(vbat, macro = u_sys, states = { on: 0.9V, off });

        #[power_domain(vdd_mon)]
        #[safety_mechanism(type = watchdog)]
        entity Watchdog {
            in clk: clock
            in kick: bit
            out timeout: bit
        }

        impl Watchdog {
            signal cnt: bit[8] = 0
            #[isolation(clamp = low)]
            signal timeout_q: bit
            on(clk.rise) {
                if kick {
                    cnt = 0
                } else {
                    cnt = cnt + 1
                }
            }
            timeout_q = cnt == 255
            timeout = timeout_q
        }

        #[power_domain(vdd_sys)]
        entity SysController {
            in clk: clock
            in kick_in: bit
            out wd_timeout: bit
        }

        impl SysController {
            inst wd = Watchdog { clk: clk, kick: kick_in }
            wd_timeout = wd.timeout
        }
        "#;
        let w = warnings(src);
        // vdd_sys can power off while vdd_mon stays up, so the nets going
        // INTO the watchdog are the ones that float — not its output.
        assert!(
            w.iter()
                .any(|m| m.contains("port `wd.kick`") && m.contains("isolation")),
            "an unrelated isolated signal must not cover `kick`: {w:#?}"
        );
        assert!(
            w.iter()
                .any(|m| m.contains("port `wd.clk`") && m.contains("isolation")),
            "an unrelated isolated signal must not cover `clk`: {w:#?}"
        );
    }

    #[test]
    fn upf_exports_isolation_and_level_shifter_strategies() {
        let src = soc(false);
        let hir = parse_and_build_hir(&src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        let upf = skalp_mir::upf::generate_upf(&hir, &mir, "Soc").expect("upf");
        assert!(
            upf.contains("set_isolation ISO_vdd_gpu -domain PD_vdd_gpu -clamp_value 0"),
            "missing set_isolation:\n{upf}"
        );
        assert!(
            upf.contains("set_isolation_control ISO_vdd_gpu")
                && upf.contains("-isolation_signal gpu_sleep")
                && upf.contains("-isolation_sense high"),
            "isolation control must assert while the domain is OFF:\n{upf}"
        );
        assert!(
            upf.contains("set_level_shifter LS_vdd_io"),
            "missing level shifter for the 1.8 V rail:\n{upf}"
        );
        assert!(
            !upf.contains("set_isolation ISO_vdd_aon"),
            "an always-on rail needs no isolation strategy:\n{upf}"
        );
    }
}

/// Retention semantics (spec §18.12).
mod retention {
    use skalp_frontend::parse_and_build_hir;

    fn design(retained_in: &str, controls: &str) -> String {
        format!(
            r#"
        power_domain vbat: external;
        power_domain vdd_aon = regulated(vbat, macro = u_aon, states = {{ on: 0.9V }});
        power_domain vdd_cpu = regulated(vbat, macro = u_cpu, states = {{ on: 0.9V, ret: 0.6V, off }});

        #[power_domain(vdd_cpu)]
        entity Cpu {{
            #[isolation(clamp = low)]
            in clk: clock
            #[isolation(clamp = low)]
            in d: bit[8]
            #[isolation(clamp = low)]
            out q: bit[8]
        }}

        impl Cpu {{
            {cpu_ret}
            signal state: bit[8] = 0
            signal local_save: bit = 0
            signal local_restore: bit = 0
            on(clk.rise) {{ state = d }}
            q = state
        }}

        #[power_domain(vdd_aon)]
        entity Soc {{
            in clk: clock
            in d: bit[8]
            out q: bit[8]
        }}

        impl Soc {{
            {soc_ret}
            signal pmu_save: bit = 0
            signal pmu_restore: bit = 0
            signal housekeeping: bit = 0
            on(clk.rise) {{ housekeeping = !housekeeping }}
            inst c = Cpu {{ clk: clk, d: d }}
            q = c.q
        }}
        "#,
            cpu_ret = if retained_in == "cpu" {
                format!("#[retention({})]", controls)
            } else {
                String::new()
            },
            soc_ret = if retained_in == "soc" {
                format!("#[retention({})]", controls)
            } else {
                String::new()
            },
        )
    }

    fn findings(src: &str) -> (Vec<String>, Vec<String>) {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::check_power_domains(&hir)
    }

    #[test]
    fn retention_in_an_always_on_domain_is_pointless() {
        // The annotation lands on Soc's `housekeeping`, in the always-on rail.
        let (errors, warnings) = findings(&design("soc", "strategy = shadow"));
        assert!(errors.is_empty(), "{errors:#?}");
        assert!(
            warnings
                .iter()
                .any(|w| w.contains("never powers off") && w.contains("vdd_aon")),
            "retention on an always-on rail must be flagged: {warnings:#?}"
        );
    }

    #[test]
    fn declared_retention_state_with_nothing_retained_is_flagged() {
        // vdd_cpu declares `ret: 0.6V` but nothing in it is retained.
        let (_, warnings) = findings(&design("soc", "strategy = shadow"));
        assert!(
            warnings.iter().any(|w| w.contains("vdd_cpu")
                && w.contains("reduced-voltage state")
                && w.contains("does not implement")),
            "an unused retention state must be flagged: {warnings:#?}"
        );
    }

    #[test]
    fn retention_control_from_inside_the_retained_domain_fails_the_build() {
        let (errors, _) = findings(&design(
            "cpu",
            "strategy = shadow, save = c.local_save, restore = c.local_restore",
        ));
        assert!(
            errors.iter().any(|e| e.contains("retention save control")
                && e.contains("inside the domain being retained")),
            "a control that dies with the state it preserves must fail: {errors:#?}"
        );
    }

    #[test]
    fn retention_from_an_always_on_controller_is_clean() {
        let (errors, warnings) = findings(&design(
            "cpu",
            "strategy = shadow, save = pmu_save, restore = pmu_restore",
        ));
        assert!(errors.is_empty(), "{errors:#?}");
        assert!(
            !warnings.iter().any(|w| w.contains("retention")),
            "a correctly-retained domain must be quiet: {warnings:#?}"
        );
    }

    #[test]
    fn upf_exports_retention_strategy_and_control() {
        let src = design(
            "cpu",
            "strategy = shadow, save = pmu_save, restore = pmu_restore",
        );
        let hir = parse_and_build_hir(&src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        let upf = skalp_mir::upf::generate_upf(&hir, &mir, "Soc").expect("upf");
        assert!(
            upf.contains("set_retention RET_vdd_cpu -domain PD_vdd_cpu")
                && upf.contains("Cpu/state"),
            "missing set_retention with elements:\n{upf}"
        );
        assert!(
            upf.contains("set_retention_control RET_vdd_cpu")
                && upf.contains("-save_signal {pmu_save high}")
                && upf.contains("-restore_signal {pmu_restore low}"),
            "missing set_retention_control:\n{upf}"
        );
        assert!(
            !upf.contains("set_retention RET_vdd_aon"),
            "nothing is retained in the always-on rail:\n{upf}"
        );
    }

    /// A power-intent attribute must not survive the construct it was written
    /// in: an attribute node can be visited more than once, and a leftover
    /// silently attached itself to the FIRST port of the NEXT entity.
    #[test]
    fn power_attributes_do_not_leak_across_entities() {
        let src = r#"
        entity A { in clk: clock  out q: bit }
        impl A {
            #[retention]
            signal s: bit = 0
            on(clk.rise) { s = !s }
            q = s
        }
        entity B { in clk: clock  out z: bit }
        impl B {
            signal t: bit = 0
            on(clk.rise) { t = !t }
            z = t
        }
        "#;
        let hir = parse_and_build_hir(src).expect("parse");
        let b = hir.entities.iter().find(|e| e.name == "B").expect("B");
        assert!(
            b.ports.iter().all(|p| p.retention_config.is_none()),
            "#[retention] in A's impl must not attach to B's ports"
        );
        let a_impl = hir
            .implementations
            .iter()
            .find(|i| {
                hir.entities
                    .iter()
                    .any(|e| e.id == i.entity && e.name == "A")
            })
            .expect("impl A");
        assert!(
            a_impl.signals.iter().any(|s| s.name == "s"
                && s.power_config
                    .as_ref()
                    .and_then(|pc| pc.retention.as_ref())
                    .is_some()),
            "the annotation must still reach the signal it was written on"
        );
    }
}

/// `#[trace(...)]` presentation must reach the exported waveform.
mod trace_metadata {
    /// The waveform writer has always had an `add_traced_signal` API — and
    /// NOTHING called it, because SIR dropped `trace_config` on the way from
    /// MIR. Every exported file therefore had empty groups and HDL names,
    /// while the tutorial promised the grouping "travels with the source".
    #[tokio::test]
    async fn trace_group_and_display_name_reach_the_waveform() {
        let src = r#"
entity Traced {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Traced {
    #[trace(group = "core", display_name = "Accumulator", radix = hex)]
    signal acc: bit[8] = 0
    on(clk.rise) { acc = d }
    q = acc
}
"#;
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("traced.sk");
        std::fs::write(&path, src).unwrap();

        let mut tb = skalp_testing::testbench::Testbench::new(path.to_str().unwrap())
            .await
            .unwrap();
        tb.set("d", 0x5Au8);
        tb.clock(2).await;

        let out = dir.path().join("traced.skw");
        tb.export_waveform(&out).unwrap();

        // The exporter always gzips, whatever the extension.
        let bytes = std::fs::read(&out).expect("read waveform");
        let mut text = String::new();
        {
            use std::io::Read;
            flate2::read::GzDecoder::new(&bytes[..])
                .read_to_string(&mut text)
                .expect("gunzip waveform");
        }

        assert!(
            text.contains("Accumulator"),
            "display_name must reach the waveform:\n{}",
            &text[..text.len().min(600)]
        );
        assert!(
            text.contains("core"),
            "group must reach the waveform:\n{}",
            &text[..text.len().min(600)]
        );
    }
}

/// `#[memory]` element width in the emitted SystemVerilog.
mod memory_attribute {
    /// `#[memory]` fell back to the width of the WHOLE array when the config
    /// omitted an explicit width, so a `bit[32][1024]` memory was declared as
    /// `reg [32767:0] ram [0:1023]` — 1024 words of 32 kbit each. Without the
    /// attribute the same signal emitted the correct `reg [31:0]`, so the
    /// annotation actively made the output wrong.
    #[test]
    fn memory_declares_element_width_not_array_width() {
        let src = r#"
entity Mem {
    in clk: clock
    in we: bit
    in addr: bit[10]
    in wdata: bit[32]
    out rdata: bit[32]
}

impl Mem {
    #[memory(depth = 1024)]
    signal ram: bit[32][1024]
    on(clk.rise) {
        if we { ram[addr] = wdata }
    }
    rdata = ram[addr]
}
"#;
        let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

        assert!(
            sv.contains("reg [31:0] ram [0:1023];"),
            "memory must declare the ELEMENT width:\n{sv}"
        );
        assert!(
            !sv.contains("[32767:0]"),
            "array-wide element width regressed:\n{sv}"
        );
        assert!(
            sv.contains("width=32"),
            "the memory comment must report the element width too:\n{sv}"
        );
    }
}

/// Capability-table claims that turned out to be wrong.
mod spec_capability_claims {
    /// The spec says octal is "not lexed; use decimal/hex/binary", which
    /// reads as "you get an error". In fact `0o52` lexed as `0` followed by
    /// the identifier `o52`, and the design built cleanly with the constant
    /// silently turned into 0 — 42 became 0 with no diagnostic at all.
    #[test]
    fn octal_literal_is_rejected_not_silently_zero() {
        let src = r#"
entity O {
    in clk: clock
    out q: bit[8]
}

impl O {
    q = 0o52
}
"#;
        let result = skalp_frontend::parse_and_build_hir(src);
        assert!(result.is_err(), "0o52 must be a hard error, not a silent 0");
    }

    /// Decimal, hex and binary keep working — the octal rule must not eat
    /// a leading `0` from anything else.
    #[test]
    fn supported_radices_still_lex() {
        let src = r#"
entity R {
    in clk: clock
    out a: bit[8]
    out b: bit[8]
    out c: bit[8]
    out d: bit[8]
}

impl R {
    a = 42
    b = 0x2A
    c = 0b101010
    d = 0
}
"#;
        skalp_frontend::parse_and_build_hir(src).expect("decimal/hex/binary/zero must still parse");
    }
}

/// `#[memory(style = ...)]` names, and what an unrecognized one does.
mod memory_style_names {
    fn design(style: &str) -> String {
        format!(
            r#"
entity M {{
    in clk: clock
    in we: bit
    in addr: bit[8]
    in din: bit[32]
    out dout: bit[32]
}}
impl M {{
    #[memory(style = {style}, depth = 256)]
    signal ram: bit[32][256]
    on(clk.rise) {{ if we {{ ram[addr] = din }} }}
    dout = ram[addr]
}}
"#
        )
    }

    fn sv(style: &str) -> String {
        let hir = skalp_frontend::parse_and_build_hir(&design(style)).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv")
    }

    /// Every style the guide documents must reach the emitted attribute.
    /// `registers` (the documented spelling, and the one the attribute
    /// itself uses) was not accepted — only the singular `register` was —
    /// so it fell through to the catch-all and silently left the memory on
    /// Auto with no attribute at all.
    #[test]
    fn documented_styles_emit_their_ram_style() {
        for (style, expect) in [
            ("block", "block"),
            ("distributed", "distributed"),
            ("ultra", "ultra"),
            ("registers", "registers"),
            ("register", "registers"),
        ] {
            let out = sv(style);
            assert!(
                out.contains(&format!("ram_style = \"{expect}\"")),
                "style `{style}` must emit ram_style=\"{expect}\":\n{out}"
            );
        }
    }

    /// An unrecognized style is a typo, and a typo that silently downgrades
    /// to Auto is the worst outcome — it looks like it worked.
    #[test]
    fn unknown_style_is_an_error_not_a_silent_downgrade() {
        let err = skalp_frontend::parse_and_build_hir(&design("blockram"))
            .expect_err("unknown memory style must fail");
        let text = format!("{err:?}");
        assert!(
            text.contains("unknown style"),
            "diagnostic must name the problem: {text}"
        );
    }
}

/// Attribute arguments must not be silently ignored.
///
/// Three of this session's defects came from one shape: a `match` over
/// attribute-argument tokens whose unknown arm is `_ => {}`. A typo'd key
/// or value was accepted and dropped, so the annotation looked applied and
/// did nothing — the worst possible outcome for an attribute whose entire
/// job is to change behaviour.
mod attribute_argument_typos {
    fn build(attr: &str) -> Result<(), String> {
        let src = format!(
            r#"
entity P {{
    in clk: clock
    out q: bit[4]
}}

impl P {{
    #[{attr}]
    signal s: bit[4] = 0
    on(clk.rise) {{ s = s + 1 }}
    q = s
}}
"#
        );
        skalp_frontend::parse_and_build_hir(&src)
            .map(|_| ())
            .map_err(|e| format!("{e:?}"))
    }

    #[test]
    fn unknown_argument_values_are_rejected() {
        for attr in [
            "trace(radix = bogus)",
            "trace(bogus_key = 3)",
            "isolation(clamp = bogus)",
            "retention(strategy = bogus)",
            "memory(depth = 4, style = bogus)",
            "cdc(cdc_type = bogus)",
            "breakpoint(is_error = bogus)",
            "breakpoint(bogus_key = 3)",
        ] {
            assert!(
                build(attr).is_err(),
                "`#[{attr}]` must be rejected, not silently ignored"
            );
        }
    }

    #[test]
    fn every_valid_form_still_builds() {
        for attr in [
            "trace",
            "trace(group = \"g\", display_name = \"D\", radix = hex)",
            "trace(radix = binary)",
            "trace(radix = ascii)",
            "isolation",
            "isolation(clamp = low)",
            "isolation(clamp = high)",
            "isolation(clamp = latch)",
            "retention",
            "retention(strategy = auto)",
            "retention(strategy = shadow, save = save_req, restore = restore_req)",
            "retention(strategy = balloon)",
            "memory(depth = 4)",
            "memory(depth = 4, style = block)",
            "memory(depth = 4, style = distributed)",
            "memory(depth = 4, style = ultra)",
            "memory(depth = 4, style = registers)",
            "cdc",
            "cdc(sync_stages = 3)",
            "cdc(cdc_type = gray)",
            "cdc(cdc_type = two_ff)",
            "cdc(cdc_type = handshake)",
            "cdc(cdc_type = pulse)",
            "cdc(cdc_type = async_fifo)",
            "cdc(from = fast, to = slow)",
            "breakpoint",
            "breakpoint(is_error = true)",
            "breakpoint(is_error = false)",
            "breakpoint(is_error = true, name = \"N\", message = \"M\")",
            "breakpoint(condition = \"x > 3\")",
        ] {
            assert!(
                build(attr).is_ok(),
                "`#[{attr}]` is valid and must still build"
            );
        }
    }
}

/// Capability-table rows that said "not implemented" but were accepted.
mod documented_as_absent {
    fn build(src: &str) -> Result<(), String> {
        skalp_frontend::parse_and_build_hir(src)
            .map_err(|e| format!("{e:?}"))
            .and_then(|hir| {
                skalp_mir::MirCompiler::new()
                    .compile_to_mir(&hir)
                    .map(|_| ())
                    .map_err(|e| e.to_string())
            })
    }

    /// A `while` whose condition cannot be analyzed for compile-time
    /// unrolling used to drop its whole body, emitting an EMPTY always block
    /// with the assigned signal never driven — behind an eprintln warning.
    #[test]
    fn unanalyzable_while_is_an_error_not_an_empty_block() {
        let src = r#"
entity W {
    in clk: clock
    out q: bit[8]
}

impl W {
    signal i: bit[8] = 0
    on(clk.rise) {
        while i < 4 { i = i + 1 }
    }
    q = i
}
"#;
        let err = build(src).expect_err("unanalyzable while must fail the build");
        assert!(
            err.contains("while loop"),
            "diagnostic must name the construct: {err}"
        );
    }

    /// The spec says while loops are "not synthesizable", but an analyzable
    /// one IS unrolled. That must keep working — the error above must not
    /// swallow the supported form.
    #[test]
    fn analyzable_while_still_unrolls() {
        let src = r#"
entity W2 {
    in clk: clock
    out q: bit[8]
}

impl W2 {
    signal acc: bit[8] = 0
    on(clk.rise) {
        let mut i: bit[8] = 0
        while i < 4 { i = i + 1 }
        acc = i
    }
    q = acc
}
"#;
        build(src).expect("an analyzable while loop must still unroll");
    }

    /// `ncl<N>` has HIR/MIR/codegen support that nothing reaches — the
    /// parser never emits NclType — so it fell through to Custom("ncl") and
    /// MIR mapped it to Bit(1): an 8-bit port silently became 1 bit.
    #[test]
    fn ncl_type_is_rejected_not_silently_one_bit() {
        let src = r#"
entity N {
    in a: ncl<8>
    out q: ncl<8>
}

impl N {
    q = a
}
"#;
        let err = build(src).expect_err("ncl<N> must be rejected");
        assert!(
            err.contains("ncl"),
            "diagnostic must name the construct: {err}"
        );
    }
}

/// The FPGA stub report names which independence claims collapse on fabric.
mod vccint_collapse {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::fpga_power::fpga_power_posture;

    fn design(mech_domain: &str) -> String {
        format!(
            r#"
        power_domain vbat: external;
        power_domain vdd_core = regulated(vbat, macro = u_core, states = {{ on: 0.9V, off }});
        power_domain vdd_mon: external;

        #[power_domain({mech_domain})]
        #[safety_mechanism(type = watchdog)]
        entity Watchdog {{
            #[isolation(clamp = low)]
            in clk: clock
            #[isolation(clamp = low)]
            in kick: bit
            out timeout: bit
        }}

        impl Watchdog {{
            signal cnt: bit[8] = 0
            on(clk.rise) {{ if kick {{ cnt = 0 }} else {{ cnt = cnt + 1 }} }}
            timeout = cnt == 255
        }}

        #[power_domain(vdd_core)]
        entity Controller {{
            in clk: clock
            in kick: bit
            out wd_timeout: bit
        }}

        impl Controller {{
            inst wd = Watchdog {{ clk: clk, kick: kick }}
            wd_timeout = wd.timeout
        }}
        "#
        )
    }

    /// A blanket "fabric shares VCCINT" caveat is true but unactionable. The
    /// report must name the mechanism whose FMEDA independence claim the
    /// device breaks.
    #[test]
    fn report_names_the_mechanism_that_loses_independence() {
        let hir = parse_and_build_hir(&design("vdd_mon")).expect("parse");
        let report = fpga_power_posture(&hir, true)
            .expect("stub allowed")
            .expect("switched/regulated domains present");
        assert!(
            report.contains("`Watchdog`")
                && report.contains("vdd_mon")
                && report.contains("vdd_core"),
            "report must name the mechanism and both domains:\n{report}"
        );
        assert!(
            report.contains("does not hold here"),
            "report must state the claim is invalid on this device:\n{report}"
        );
    }

    /// A mechanism that shares a supply in the SOURCE relies on no
    /// independence, so fabric invalidates nothing — say that instead of
    /// listing it.
    #[test]
    fn nothing_to_invalidate_when_no_independence_is_claimed() {
        // vdd_core for both: the CCF check downgrades via allow_shared_supply
        // in real designs; here the mechanism simply shares its context's rail.
        let hir = parse_and_build_hir(&design("vdd_core")).expect("parse");
        let report = fpga_power_posture(&hir, true)
            .expect("stub allowed")
            .expect("switched/regulated domains present");
        assert!(
            report.contains("No #[safety_mechanism] in this design relies on supply independence"),
            "report must say nothing is invalidated:\n{report}"
        );
    }
}

/// Isolation-enable liveness (spec §18.11): the first checkable piece of
/// transition sequencing.
mod isolation_enable_liveness {
    use skalp_frontend::parse_and_build_hir;

    fn design_with_aon_states(enable_site: &str, pst: &str, aon_states: &str) -> String {
        let (gpu_sig, soc_sig, enable) = match enable_site {
            "local" => ("    signal local_iso: bit = 0\n", "", "local_iso"),
            _ => ("", "    signal iso_en: bit = 0\n", "iso_en"),
        };
        format!(
            r#"
        power_domain vbat: external;
        power_domain vdd_aon = regulated(vbat, macro = u_aon, states = {{ {aon_states} }});
        power_domain vdd_gpu = switched(vdd_aon, on_when = !gpu_sleep, states = {{ on: 0.9V, off }});

        #[power_domain(vdd_gpu)]
        entity Gpu {{
            in clk: clock
            in cmd: bit[8]
            #[isolation(clamp = low, enable = {enable})]
            out result: bit[16]
        }}

        impl Gpu {{
            signal acc: bit[16] = 0
        {gpu_sig}    on(clk.rise) {{ acc = acc + cmd }}
            result = acc
        }}

        #[power_domain(vdd_aon)]
        entity Soc {{
            in clk: clock
            in gpu_sleep: bit
            in cmd: bit[8]
            out result: bit[16]
        }}

        impl Soc {{
        {soc_sig}    inst g = Gpu {{ clk: clk, cmd: cmd }}
            result = g.result
        }}
        {pst}"#
        )
    }

    fn design(enable_site: &str, pst: &str) -> String {
        design_with_aon_states(enable_site, pst, "on: 0.9V")
    }

    fn findings(src: &str) -> (Vec<String>, Vec<String>) {
        let hir = parse_and_build_hir(src).expect("parse");
        skalp_mir::MirCompiler::check_power_domains(&hir)
    }

    /// A clamp enabled from inside the domain it clamps cannot fire when it
    /// is needed — the enable dies with the rail.
    #[test]
    fn enable_inside_the_clamped_domain_fails() {
        let (errors, _) = findings(&design("local", ""));
        assert!(
            errors
                .iter()
                .any(|e| e.contains("clamp control dies with the domain")),
            "an enable inside the clamped domain must fail: {errors:#?}"
        );
    }

    /// An enable driven from the always-on rail is the correct arrangement.
    #[test]
    fn enable_from_an_always_on_domain_is_clean() {
        let (errors, _) = findings(&design("aon", ""));
        assert!(
            !errors.iter().any(|e| e.contains("isolation on")),
            "a correctly-placed enable must not be flagged: {errors:#?}"
        );
    }

    /// With a state table, the enable's domain must be up in every state
    /// where the clamped domain is off.
    #[test]
    fn pst_state_with_both_off_is_flagged() {
        let pst = "power_states { run = { vdd_aon: on, vdd_gpu: on }, \
                   sleep = { vdd_aon: off, vdd_gpu: off } };";
        // vdd_aon must declare an `off` state for the table to reference it —
        // the PST validator rightly rejects a state a domain cannot enter.
        let (errors, _) = findings(&design_with_aon_states("aon", pst, "on: 0.9V, off"));
        assert!(
            errors
                .iter()
                .any(|e| e.contains("nothing can assert the clamp")),
            "a state with both domains off must be flagged: {errors:#?}"
        );
    }
}

/// A signal driven only by a resolved priority chain is still a register.
mod resolved_conditional_is_a_register {
    /// `is_register` decides `reg` vs `wire` in the emitted SystemVerilog,
    /// and its scan did not handle `Statement::ResolvedConditional` — the
    /// form an if-else-if chain lowers to. A signal driven only by such a
    /// chain was declared `wire` and then assigned with `<=` inside
    /// always_ff, which is invalid SystemVerilog that no SV tool accepts.
    #[test]
    fn priority_chain_target_is_declared_reg() {
        let src = r#"
entity Prio {
    in clk: clock
    in a: bit
    in b: bit
    in c: bit
    in d: bit[8]
    out q: bit[8]
}

impl Prio {
    signal r: bit[8] = 0
    on(clk.rise) {
        if a {
            r = d
        } else if b {
            r = d + 1
        } else if c {
            r = d + 2
        } else {
            r = 0
        }
    }
    q = r
}
"#;
        let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

        assert!(
            sv.contains("reg [7:0] r"),
            "a signal driven by a priority chain must be a reg:\n{sv}"
        );
        assert!(
            !sv.contains("wire [7:0] r"),
            "declaring it a wire makes the always_ff assignment invalid SV:\n{sv}"
        );
    }
}

/// Backends must not emit a placeholder comment in place of hardware.
mod backend_placeholders {
    /// The HIR statement emitters fall through to
    /// `// unsupported statement` for constructs they cannot express, so the
    /// hardware silently disappeared while the build reported success. A
    /// comment is not an implementation.
    #[test]
    fn unsupported_construct_fails_the_vhdl_backend() {
        let src = r#"
entity F {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl F {
    signal acc: bit[8] = 0
    on(clk.rise) {
        let mut i: bit[8] = 0
        while i < 4 { i = i + 1 }
        acc = d + i
    }
    q = acc
}
"#;
        let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
        let text = match skalp_hir_codegen::generate_vhdl_files(&hir) {
            Ok(_) => panic!("a construct the backend cannot express must fail the build"),
            Err(e) => format!("{e}"),
        };
        assert!(
            text.contains("placeholders") && text.contains("unsupported statement"),
            "the error must name what was dropped and where: {text}"
        );
    }

    /// The ordinary path must keep working — every tutorial VHDL design goes
    /// through this backend.
    #[test]
    fn a_supported_design_still_generates_vhdl() {
        let src = r#"
entity Counter {
    in clk: clock
    in rst: reset
    out q: bit[8]
}

impl Counter {
    signal count: bit[8] = 0
    on(clk.rise) {
        if rst {
            count = 0
        } else {
            count = count + 1
        }
    }
    q = count
}
"#;
        let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
        let files = skalp_hir_codegen::generate_vhdl_files(&hir).expect("vhdl must generate");
        assert!(!files.is_empty(), "expected at least one VHDL file");
    }
}

/// A `let` variable driven by an if-else-if chain in a clocked block.
mod resolved_conditional_ssa {
    fn sv_of(src: &str) -> String {
        let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
        let mir = skalp_mir::MirCompiler::new()
            .compile_to_mir(&hir)
            .expect("mir");
        skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv")
    }

    const SRC: &str = r#"
entity W2 {
    in clk: clock
    in a: bit
    in b: bit
    in x: bit[8]
    in y: bit[8]
    out q: bit[32]
}

impl W2 {
    signal r: bit[32] = 0
    on(clk.rise) {
        let mut v: bit[8] = 0
        if a {
            v = x ++ y ++ x ++ y
        } else if b {
            v = y ++ x ++ y ++ x
        } else {
            v = 0
        }
        r = v
    }
    q = r
}
"#;

    /// The SSA pass ignored `Statement::ResolvedConditional` in both of its
    /// counting walks and never versioned `rc.target` in the rename walk, so
    /// the chain's target kept the ORIGINAL name and collided with the `let`
    /// initializer: `v = 0;` (blocking) and `v <= ...;` (non-blocking) landed
    /// in the same always_ff. Synthesis tools reject that, and `r <= v` read
    /// the pre-update value.
    #[test]
    fn no_variable_gets_both_blocking_and_nonblocking_in_one_block() {
        let sv = sv_of(SRC);
        for blk in sv.split("always_ff").skip(1) {
            let body = blk.split("end").next().unwrap_or("");
            let mut blocking = std::collections::HashSet::new();
            let mut nonblocking = std::collections::HashSet::new();
            for line in body.lines() {
                let t = line.trim();
                if let Some((lhs, rest)) = t.split_once('=') {
                    let name = lhs.trim().trim_end_matches('<').trim();
                    if name.is_empty() || !name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        continue;
                    }
                    if t.contains("<=") {
                        nonblocking.insert(name.to_string());
                    } else if !rest.starts_with('=') {
                        blocking.insert(name.to_string());
                    }
                }
            }
            let both: Vec<_> = blocking.intersection(&nonblocking).collect();
            assert!(
                both.is_empty(),
                "mixed blocking/non-blocking on {both:?} in one always_ff:\n{sv}"
            );
        }
    }

    /// The width scan skipped ResolvedConditional too, so the chain's target
    /// kept its declared 8 bits and truncated a 32-bit concat.
    #[test]
    fn chain_target_is_widened_to_the_widest_arm() {
        let sv = sv_of(SRC);
        assert!(
            !sv.contains("logic [7:0] v_ssa"),
            "the chain target must be widened to hold a 32-bit concat:\n{sv}"
        );
    }
}
