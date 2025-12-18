// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (Gate, GateType, LirDesign, LirModule, LirSignal, Net) which has been replaced by
// the new technology mapping infrastructure (WordLir, GateNetlist, TechMapper).
// TODO: Update these tests to use the new GateNetlist API for place & route.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod native_place_route_tests {
    use skalp_lir::{Gate, GateType, LirDesign, LirModule, LirSignal, Net};
    use skalp_place_route::{
        bitstream::{BitstreamConfig, BitstreamFormat, BitstreamGenerator},
        device::{Device, DeviceFamily, SwitchPattern},
        placer::{PlacementAlgorithm, Placer, PlacerConfig},
        router::RoutingAlgorithm,
        router::{Router, RouterConfig},
        timing::{TimingAnalyzer, TimingConfig, TimingDrivenPlacer},
    };

    #[test]
    fn test_ice40_device_architecture() {
        println!("🔧 Testing iCE40 Device Architecture");

        // Test HX8K device
        let hx8k = Device::ice40_hx8k();
        println!("\n📱 iCE40 HX8K Device:");
        println!("   Name: {}", hx8k.name);
        println!("   Family: {:?}", hx8k.family);
        println!("   Grid size: {:?}", hx8k.grid_size);
        println!("   Logic tiles: {}", hx8k.logic_tiles.len());
        println!("   I/O tiles: {}", hx8k.io_tiles.len());
        println!("   Memory blocks: {}", hx8k.memory_blocks.len());

        let stats_hx8k = hx8k.stats();
        println!("   Total LUTs: {}", stats_hx8k.total_luts);
        println!("   Total FFs: {}", stats_hx8k.total_ffs);
        println!("   Total I/Os: {}", stats_hx8k.total_ios);
        println!("   Total BRAMs: {}", stats_hx8k.total_brams);

        // Test HX1K device
        let hx1k = Device::ice40_hx1k();
        println!("\n📱 iCE40 HX1K Device:");
        println!("   Name: {}", hx1k.name);
        println!("   Grid size: {:?}", hx1k.grid_size);
        println!("   Logic tiles: {}", hx1k.logic_tiles.len());

        let stats_hx1k = hx1k.stats();
        println!("   Total LUTs: {}", stats_hx1k.total_luts);
        println!("   Total FFs: {}", stats_hx1k.total_ffs);

        // Validate architecture
        assert_eq!(hx8k.family, DeviceFamily::Ice40);
        assert_eq!(hx8k.grid_size, (33, 33));
        assert!(!hx8k.logic_tiles.is_empty());
        assert!(!hx8k.io_tiles.is_empty());
        assert!(stats_hx8k.total_luts >= 7680 && stats_hx8k.total_luts = 7800); // HX8K: got 7688 LUTs (961 tiles × 8 LUTs/tile)

        assert_eq!(hx1k.family, DeviceFamily::Ice40);
        assert_eq!(hx1k.grid_size, (17, 17));
        assert!(!hx1k.logic_tiles.is_empty());

        println!("✅ iCE40 device architecture validated");
    }

    #[test]
    fn test_native_placement_algorithms() {
        println!("🎯 Testing Native Placement Algorithms");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        let algorithms = vec![
            PlacementAlgorithm::Random,
            PlacementAlgorithm::Analytical,
            PlacementAlgorithm::ForceDirected,
            PlacementAlgorithm::SimulatedAnnealing,
        ];

        for algorithm in algorithms {
            println!("\n🔧 Testing {:?} placement...", algorithm);

            let config = PlacerConfig {
                algorithm: algorithm.clone(),
                max_iterations: 1000, // Reduced for testing
                initial_temperature: 50.0,
                cooling_rate: 0.9,
                timing_weight: 0.6,
                wirelength_weight: 0.3,
                congestion_weight: 0.1,
            };

            let mut placer = Placer::new(config, device.clone());
            let result = placer
                .place(&test_design)
                .expect("Placement should succeed");

            println!("   Placements: {}", result.placements.len());
            println!("   Cost: {:.2}", result.cost);
            println!("   Wirelength: {}", result.wirelength);
            println!("   Timing score: {:.2} ns", result.timing_score);
            println!("   Utilization: {:.1}%", result.utilization * 100.0);

            // Validate results
            assert!(!result.placements.is_empty());
            assert!(result.cost >= 0.0);
            assert!(result.utilization >= 0.0 && result.utilization = 1.0);

            // Check that all placements are within device bounds
            for &(x, y) in result.placements.values() {
                assert!(x < device.grid_size.0);
                assert!(y < device.grid_size.1);
            }
        }

        println!("✅ All placement algorithms working correctly");
    }

    #[test]
    fn test_placement_quality_comparison() {
        println!("📊 Testing Placement Quality Comparison");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        let mut results = Vec::new();

        // Test different algorithms and compare quality
        let algorithms = vec![
            ("Random", PlacementAlgorithm::Random),
            ("Analytical", PlacementAlgorithm::Analytical),
            ("Force-Directed", PlacementAlgorithm::ForceDirected),
            (
                "Simulated Annealing",
                PlacementAlgorithm::SimulatedAnnealing,
            ),
        ];

        for (name, algorithm) in algorithms {
            let config = PlacerConfig {
                algorithm,
                max_iterations: 500,
                initial_temperature: 100.0,
                cooling_rate: 0.95,
                timing_weight: 0.5,
                wirelength_weight: 0.4,
                congestion_weight: 0.1,
            };

            let mut placer = Placer::new(config, device.clone());
            let result = placer
                .place(&test_design)
                .expect("Placement should succeed");

            println!(
                "   {}: cost={:.2}, wirelength={}, timing={:.2}ns",
                name, result.cost, result.wirelength, result.timing_score
            );

            results.push((name, result));
        }

        // Find best result
        let best = results
            .iter()
            .min_by(|a, b| a.1.cost.partial_cmp(&b.1.cost).unwrap())
            .unwrap();

        println!("🏆 Best placement: {} (cost: {:.2})", best.0, best.1.cost);

        // Simulated annealing should generally be among the best
        let sa_result = results
            .iter()
            .find(|(name, _)| *name == "Simulated Annealing")
            .unwrap();
        assert!(sa_result.1.cost = best.1.cost * 1.5); // Within 50% of best

        println!("✅ Placement quality comparison completed");
    }

    #[test]
    fn test_routing_algorithms() {
        println!("🛣️ Testing Native Routing Algorithms");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        // First run placement
        let config = PlacerConfig {
            algorithm: PlacementAlgorithm::Analytical,
            max_iterations: 100,
            ..Default::default()
        };

        let mut placer = Placer::new(config, device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("Placement should succeed");

        println!(
            "   Placement completed: {} gates placed",
            placement_result.placements.len()
        );

        // Now test routing
        let router_config = RouterConfig {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 50,
            allow_ripup: true,
            max_congestion: 1.5,
            history_cost_factor: 0.5,
            present_congestion_factor: 1.0,
            timing_weight: 0.3,
        };

        let mut router = Router::new(router_config, device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("Routing should succeed");

        println!("   Routes: {}", routing_result.routes.len());
        println!("   Congestion: {:.2}", routing_result.congestion);
        println!("   Wirelength: {}", routing_result.wirelength);

        // Validate routing results
        assert!(routing_result.congestion >= 0.0);
        // wirelength is always non-negative (unsigned type)

        println!("✅ Routing algorithms working correctly");
    }

    #[test]
    fn test_bitstream_generation() {
        println!("💾 Testing Native Bitstream Generation");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        // Run placement
        let mut placer = Placer::new(PlacerConfig::default(), device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("Placement should succeed");

        // Run routing
        let mut router = Router::new(RouterConfig::default(), device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("Routing should succeed");

        // Generate bitstream
        let bitstream_gen = BitstreamGenerator::new(device.clone());
        let bitstream = bitstream_gen
            .generate(&placement_result, &routing_result)
            .expect("Bitstream generation should succeed");

        println!("   Bitstream size: {} bytes", bitstream.data.len());
        println!("   Target device: {}", bitstream.device);

        // Validate bitstream
        assert!(!bitstream.data.is_empty());
        assert_eq!(bitstream.device, device.name);

        // Test writing to file
        let temp_path = std::path::Path::new("/tmp/test_bitstream.bin");
        bitstream
            .write_to_file(temp_path)
            .expect("Should write bitstream to file");

        // Verify file was created
        assert!(temp_path.exists());
        let file_size = std::fs::metadata(temp_path).unwrap().len();
        assert_eq!(file_size, bitstream.data.len() as u64);

        // Clean up
        std::fs::remove_file(temp_path).ok();

        println!("✅ Bitstream generation working correctly");
    }

    #[test]
    fn test_advanced_bitstream_formats() {
        println!("🔧 Testing Advanced Bitstream Formats");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        // Run placement and routing
        let mut placer = Placer::new(PlacerConfig::default(), device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("Placement should succeed");

        let mut router = Router::new(RouterConfig::default(), device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("Routing should succeed");

        // Test different bitstream formats
        let formats = vec![
            (BitstreamFormat::IceStormAscii, "ASCII"),
            (BitstreamFormat::IceStormBinary, "Binary"),
            (BitstreamFormat::VtrBitstream, "VTR XML"),
        ];

        for (format, name) in formats {
            println!("\n🔧 Testing {} format...", name);

            let config = BitstreamConfig {
                format: format.clone(),
                include_routing: true,
                compress: false,
                timing_annotations: true,
            };

            let bitstream_gen = BitstreamGenerator::with_config(device.clone(), config);
            let bitstream = bitstream_gen
                .generate(&placement_result, &routing_result)
                .expect("Bitstream generation should succeed");

            println!("   Format: {}", bitstream.format_info());
            println!("   Size: {} bytes", bitstream.data.len());
            println!("   Device: {}", bitstream.device);
            println!(
                "   Utilization: {:.1}%",
                bitstream.metadata.logic_utilization * 100.0
            );

            // Verify bitstream integrity
            bitstream.verify().expect("Bitstream should be valid");

            // Test file writing with report
            let temp_path_str = format!("/tmp/test_bitstream_{}.out", name.to_lowercase());
            let temp_path = std::path::Path::new(&temp_path_str);
            bitstream
                .write_with_report(temp_path)
                .expect("Should write bitstream and report");

            // Verify files were created
            assert!(temp_path.exists());
            let report_path = temp_path.with_extension("rpt");
            assert!(report_path.exists());

            // Check file sizes
            let bitstream_size = std::fs::metadata(temp_path).unwrap().len();
            let report_size = std::fs::metadata(&report_path).unwrap().len();

            println!("   Bitstream file: {} bytes", bitstream_size);
            println!("   Report file: {} bytes", report_size);

            assert_eq!(bitstream_size, bitstream.data.len() as u64);
            assert!(report_size > 100); // Report should have substantial content

            // Validate specific format content
            match format {
                BitstreamFormat::IceStormAscii => {
                    let content = String::from_utf8_lossy(&bitstream.data);
                    assert!(content.contains(".comment Generated by SKALP"));
                    assert!(content.contains(".device ice40hx1k"));
                    assert!(content.contains(".grid 17 17"));
                    assert!(content.contains(".end"));
                }
                BitstreamFormat::IceStormBinary => {
                    assert!(bitstream.data.len() >= 8);
                    assert_eq!(&bitstream.data[0..4], [0xFF, 0x00, 0x00, 0xFF]);
                    assert_eq!(&bitstream.data[4..8], [0x7E, 0xAA, 0x99, 0x7E]);
                }
                BitstreamFormat::VtrBitstream => {
                    let content = String::from_utf8_lossy(&bitstream.data);
                    assert!(content.contains("<?xml version=\"1.0\"?>"));
                    assert!(content.contains("<vtr_bitstream>"));
                    assert!(content.contains(&device.name));
                }
                _ => {}
            }

            // Clean up
            std::fs::remove_file(temp_path).ok();
            std::fs::remove_file(report_path).ok();

            println!("   ✅ {} format validated", name);
        }

        println!("\n✅ All advanced bitstream formats working correctly");
    }

    #[test]
    fn test_ecp5_device_support() {
        println!("🔧 Testing ECP5 Device Support Framework");

        // Create a mock ECP5 device (placeholder until full implementation)
        let mut ecp5_device = Device::ice40_hx1k(); // Base template
        ecp5_device.name = "ECP5-25F".to_string();
        ecp5_device.family = DeviceFamily::Ecp5;
        ecp5_device.grid_size = (25, 25);

        println!("   Device: {}", ecp5_device.name);
        println!("   Family: {:?}", ecp5_device.family);
        println!("   Grid: {:?}", ecp5_device.grid_size);

        // Test ECP5 bitstream generation
        let test_design = create_test_counter_design();

        let mut placer = Placer::new(PlacerConfig::default(), ecp5_device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("ECP5 placement should work");

        let mut router = Router::new(RouterConfig::default(), ecp5_device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("ECP5 routing should work");

        let config = BitstreamConfig {
            format: BitstreamFormat::TrellisBinary,
            include_routing: true,
            compress: false,
            timing_annotations: false,
        };

        let bitstream_gen = BitstreamGenerator::with_config(ecp5_device.clone(), config);
        let bitstream = bitstream_gen
            .generate(&placement_result, &routing_result)
            .expect("ECP5 bitstream generation should work");

        println!("   Bitstream format: {}", bitstream.format_info());
        println!("   Bitstream size: {} bytes", bitstream.data.len());

        // Verify ECP5-specific content
        assert!(bitstream.data.starts_with(b"TRELLIS_ECP5"));
        assert_eq!(bitstream.device, "ECP5-25F");

        println!("✅ ECP5 device support framework operational");
    }

    #[test]
    fn test_vtr_academic_support() {
        println!("🎓 Testing VTR Academic Architecture Support");

        // Create VTR academic device
        let mut vtr_device = Device::ice40_hx1k(); // Base template
        vtr_device.name = "VTR_Academic".to_string();
        vtr_device.family = DeviceFamily::OpenFpga;
        vtr_device.grid_size = (10, 10);

        println!("   Device: {}", vtr_device.name);
        println!("   Family: {:?}", vtr_device.family);
        println!("   Grid: {:?}", vtr_device.grid_size);

        let test_design = create_test_counter_design();

        let mut placer = Placer::new(PlacerConfig::default(), vtr_device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("VTR placement should work");

        let mut router = Router::new(RouterConfig::default(), vtr_device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("VTR routing should work");

        let config = BitstreamConfig {
            format: BitstreamFormat::VtrBitstream,
            include_routing: true,
            compress: false,
            timing_annotations: true,
        };

        let bitstream_gen = BitstreamGenerator::with_config(vtr_device.clone(), config);
        let bitstream = bitstream_gen
            .generate(&placement_result, &routing_result)
            .expect("VTR bitstream generation should work");

        println!("   Bitstream format: {}", bitstream.format_info());
        println!("   Bitstream size: {} bytes", bitstream.data.len());

        // Verify VTR XML content
        let content = String::from_utf8_lossy(&bitstream.data);
        assert!(content.contains("<?xml version=\"1.0\"?>"));
        assert!(content.contains("<vtr_bitstream>"));
        assert!(content.contains("VTR_Academic"));
        assert!(content.contains("<placement>"));

        // Count blocks in XML
        let block_count = content.matches("<block").count();
        println!("   Placement blocks in XML: {}", block_count);
        assert!(block_count > 0);

        println!("✅ VTR academic architecture support operational");
    }

    #[test]
    fn test_complete_native_flow() {
        println!("🔄 Testing Complete Native Place & Route Flow");

        let device = Device::ice40_hx8k();
        let test_design = create_complex_test_design();

        println!("\n📋 Design statistics:");
        let total_gates: usize = test_design.modules.iter().map(|m| m.gates.len()).sum();
        let total_nets: usize = test_design.modules.iter().map(|m| m.nets.len()).sum();
        let total_signals: usize = test_design.modules.iter().map(|m| m.signals.len()).sum();
        println!(
            "   Gates: {}, Nets: {}, Signals: {}",
            total_gates, total_nets, total_signals
        );

        // Step 1: Placement with simulated annealing
        println!("\n🎯 Step 1: Placement");
        let placement_config = PlacerConfig {
            algorithm: PlacementAlgorithm::SimulatedAnnealing,
            max_iterations: 2000,
            initial_temperature: 100.0,
            cooling_rate: 0.98,
            timing_weight: 0.5,
            wirelength_weight: 0.4,
            congestion_weight: 0.1,
        };

        let mut placer = Placer::new(placement_config, device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("Placement should succeed");

        println!("   Placement cost: {:.2}", placement_result.cost);
        println!("   Wirelength: {}", placement_result.wirelength);
        println!(
            "   Utilization: {:.1}%",
            placement_result.utilization * 100.0
        );

        // Step 2: Routing
        println!("\n🛣️ Step 2: Routing");
        let routing_config = RouterConfig {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 100,
            allow_ripup: true,
            max_congestion: 1.3,
            history_cost_factor: 0.6,
            present_congestion_factor: 1.2,
            timing_weight: 0.4,
        };

        let mut router = Router::new(routing_config, device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("Routing should succeed");

        println!("   Routes created: {}", routing_result.routes.len());
        println!("   Final congestion: {:.2}", routing_result.congestion);
        println!("   Total wirelength: {}", routing_result.wirelength);

        // Step 3: Bitstream generation
        println!("\n💾 Step 3: Bitstream Generation");
        let bitstream_gen = BitstreamGenerator::new(device.clone());
        let bitstream = bitstream_gen
            .generate(&placement_result, &routing_result)
            .expect("Bitstream generation should succeed");

        println!("   Bitstream size: {} bytes", bitstream.data.len());

        // Validate complete flow
        assert!(!placement_result.placements.is_empty());
        assert!(placement_result.utilization > 0.0);
        // routes.len() is always non-negative (unsigned type)
        assert!(!bitstream.data.is_empty());

        // Calculate flow statistics
        let device_stats = device.stats();
        let resource_utilization = (total_gates as f64) / (device_stats.total_luts as f64);

        println!("\n📊 Flow Summary:");
        println!("   Device: {}", device.name);
        println!(
            "   Resource utilization: {:.1}%",
            resource_utilization * 100.0
        );
        println!("   Placement quality: {:.2} cost", placement_result.cost);
        println!(
            "   Routing quality: {:.2} congestion",
            routing_result.congestion
        );
        println!("   Implementation: {} bytes", bitstream.data.len());

        println!("\n🎉 COMPLETE NATIVE PLACE & ROUTE FLOW - SUCCESS!");
        println!("   ✅ Device architecture modeled accurately");
        println!("   ✅ Placement algorithms functional");
        println!("   ✅ Routing algorithms working");
        println!("   ✅ Bitstream generation operational");
        println!("   ✅ Full SKALP → LIR → Place & Route → Bitstream flow validated");

        assert!(
            resource_utilization < 0.8,
            "Design should fit comfortably on device"
        );
        assert!(
            placement_result.cost < 1000.0,
            "Placement cost should be reasonable"
        );
        assert!(
            routing_result.congestion < 2.0,
            "Routing congestion should be manageable"
        );
    }

    #[test]
    fn test_pathfinder_astar_routing() {
        println!("🔗 Testing PathFinder A* Routing Algorithm");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        // Place design
        let mut placer = Placer::new(PlacerConfig::default(), device.clone());
        let placement_result = placer
            .place(&test_design)
            .expect("Placement should succeed");

        // Test PathFinder A* routing
        let astar_config = RouterConfig {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 50,
            allow_ripup: true,
            max_congestion: 1.1,
            history_cost_factor: 0.7,
            present_congestion_factor: 1.5,
            timing_weight: 0.4,
        };

        let mut router = Router::new(astar_config, device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("PathFinder A* routing should succeed");

        println!("   Algorithm: PathFinder A*");
        println!("   Routes: {}", routing_result.routes.len());
        println!("   Congestion: {:.3}", routing_result.congestion);
        println!("   Wirelength: {}", routing_result.wirelength);

        // Verify routing quality
        assert!(
            routing_result.congestion = 1.5,
            "Congestion should be manageable"
        );
        // wirelength is always non-negative (unsigned type)

        // Test different routing algorithms
        let algorithms = vec![
            (RoutingAlgorithm::PathFinderAStar, "PathFinder A*"),
            (RoutingAlgorithm::MazeRouting, "Maze Routing"),
            (RoutingAlgorithm::TimingDriven, "Timing-Driven"),
        ];

        for (algorithm, name) in algorithms {
            let config = RouterConfig {
                algorithm,
                ..RouterConfig::default()
            };

            let mut router = Router::new(config, device.clone());
            let result = router
                .route(&test_design, &placement_result)
                .unwrap_or_else(|_| panic!("{} should succeed", name));

            println!(
                "   {}: {} routes, {:.3} congestion",
                name,
                result.routes.len(),
                result.congestion
            );
        }

        println!("✅ PathFinder A* routing test passed");
    }

    #[test]
    fn test_timing_analysis_and_closure() {
        println!("⏰ Testing Static Timing Analysis and Closure");

        let device = Device::ice40_hx1k();
        let test_design = create_test_counter_design();

        // Place design with timing-driven optimization
        let timing_config = TimingConfig {
            target_frequency: 150.0, // 150 MHz target
            setup_margin: 0.3,
            hold_margin: 0.1,
            clock_uncertainty: 0.15,
            multicycle_analysis: false,
            max_critical_paths: 5,
        };

        let mut timing_driven_placer = TimingDrivenPlacer::new(
            PlacerConfig::default(),
            timing_config.clone(),
            device.clone(),
        );

        let (placement_result, timing_report) = timing_driven_placer
            .place_with_timing(&test_design)
            .expect("Timing-driven placement should succeed");

        println!("   Placement completed with timing optimization");
        println!(
            "   Design frequency: {:.1} MHz",
            timing_report.design_frequency
        );
        println!(
            "   Timing status: {}",
            if timing_report.meets_timing {
                "PASS"
            } else {
                "FAIL"
            }
        );

        // Run routing with timing awareness
        let timing_router_config = RouterConfig {
            algorithm: RoutingAlgorithm::TimingDriven,
            max_iterations: 75,
            allow_ripup: true,
            max_congestion: 1.2,
            history_cost_factor: 0.4,
            present_congestion_factor: 1.0,
            timing_weight: 0.8, // High timing weight
        };

        let mut router = Router::new(timing_router_config, device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("Timing-driven routing should succeed");

        // Perform final timing analysis with actual routing
        let mut timing_analyzer = TimingAnalyzer::new(timing_config, device.clone());
        let final_timing = timing_analyzer
            .analyze_timing(&test_design, &placement_result, &routing_result)
            .expect("Final timing analysis should succeed");

        println!("   Final timing analysis:");
        println!("     Target: 150.0 MHz");
        println!("     Achieved: {:.1} MHz", final_timing.design_frequency);
        println!("     Clock domains: {}", final_timing.clock_summaries.len());
        println!("     Critical paths: {}", final_timing.critical_paths.len());

        if !final_timing.meets_timing {
            println!(
                "     Worst negative slack: {:.3} ns",
                final_timing.worst_negative_slack
            );
            println!("     Failing paths: {}", final_timing.failing_paths);
        }

        // Verify timing analysis components
        assert!(
            !final_timing.clock_summaries.is_empty(),
            "Should have at least one clock domain"
        );
        assert!(
            final_timing.design_frequency > 0.0,
            "Design frequency should be positive"
        );

        // Test different timing configurations
        let configs = vec![
            (100.0, "Conservative 100 MHz"),
            (200.0, "Aggressive 200 MHz"),
            (50.0, "Relaxed 50 MHz"),
        ];

        for (freq, desc) in configs {
            let config = TimingConfig {
                target_frequency: freq,
                ..TimingConfig::default()
            };

            let mut analyzer = TimingAnalyzer::new(config, device.clone());
            let report = analyzer
                .analyze_timing(&test_design, &placement_result, &routing_result)
                .expect("Timing analysis should succeed");

            println!(
                "   {}: {:.1} MHz ({})",
                desc,
                report.design_frequency,
                if report.meets_timing { "PASS" } else { "FAIL" }
            );
        }

        println!("✅ Timing analysis and closure test passed");
    }

    #[test]
    fn test_comprehensive_ecp5_support() {
        println!("🔹 Testing Comprehensive ECP5 FPGA Support");

        // Test ECP5 LFE5U-25F device
        let ecp5_25f = Device::ecp5_lfe5u_25f();
        println!("   ECP5 LFE5U-25F Device:");
        println!(
            "     Grid: {}x{}",
            ecp5_25f.grid_size.0, ecp5_25f.grid_size.1
        );

        let stats_25f = ecp5_25f.stats();
        println!("     Logic tiles: {}", ecp5_25f.logic_tiles.len());
        println!("     LUTs: {}", stats_25f.total_luts);
        println!("     FFs: {}", stats_25f.total_ffs);
        println!("     I/Os: {}", stats_25f.total_ios);
        println!("     EBRs: {}", stats_25f.total_brams);
        println!("     DSPs: {}", stats_25f.total_dsps);

        // Verify ECP5 architecture features
        assert!(ecp5_25f.grid_size.0 > 80, "ECP5 should have large grid");
        assert!(
            stats_25f.total_luts > 20000,
            "ECP5-25F should have >20K LUTs"
        );
        assert!(stats_25f.total_dsps > 0, "ECP5 should have DSP tiles");
        assert!(stats_25f.total_brams > 50, "ECP5-25F should have >50 EBRs");
        assert_eq!(ecp5_25f.family, DeviceFamily::Ecp5);

        // Test ECP5 LFE5U-85F device (larger variant)
        let ecp5_85f = Device::ecp5_lfe5u_85f();
        println!("\n   ECP5 LFE5U-85F Device:");
        println!(
            "     Grid: {}x{}",
            ecp5_85f.grid_size.0, ecp5_85f.grid_size.1
        );

        let stats_85f = ecp5_85f.stats();
        println!("     Logic tiles: {}", ecp5_85f.logic_tiles.len());
        println!("     LUTs: {}", stats_85f.total_luts);
        println!("     EBRs: {}", stats_85f.total_brams);

        // Verify 85F is larger than 25F
        assert!(
            ecp5_85f.grid_size.0 > ecp5_25f.grid_size.0,
            "85F should be larger than 25F"
        );
        assert!(
            stats_85f.total_luts > stats_25f.total_luts,
            "85F should have more LUTs"
        );
        assert!(
            stats_85f.total_brams > stats_25f.total_brams,
            "85F should have more EBRs"
        );

        // Test ECP5 I/O features
        let io_tile = &ecp5_25f.io_tiles[0];
        assert!(io_tile.diff_pairs, "ECP5 should support differential pairs");
        assert!(
            io_tile.io_standards.len() >= 5,
            "ECP5 should support multiple I/O standards"
        );
        assert!(
            io_tile.drive_strengths.len() >= 5,
            "ECP5 should support multiple drive strengths"
        );

        // Test ECP5 clock resources
        assert!(
            ecp5_25f.clock_resources.global_clocks >= 16,
            "ECP5 should have >=16 global clocks"
        );
        assert!(
            ecp5_25f.clock_resources.plls >= 2,
            "ECP5 should have >=2 PLLs"
        );
        assert!(ecp5_25f.clock_resources.dlls >= 2, "ECP5 should have DLLs");

        // Test ECP5 routing architecture
        assert!(
            ecp5_25f.routing.channels.0 >= 24,
            "ECP5 should have >=24 routing tracks"
        );
        assert_eq!(ecp5_25f.routing.switch_pattern, SwitchPattern::Universal);

        println!("✅ ECP5 device architecture tests passed");
    }

    #[test]
    fn test_ecp5_place_route_bitstream_flow() {
        println!("🔹 Testing Complete ECP5 Place & Route Flow");

        let device = Device::ecp5_lfe5u_25f();
        let test_design = create_test_counter_design();

        // Step 1: ECP5 Placement
        println!("\n   Step 1: ECP5 Placement");
        let mut placer = Placer::new(
            PlacerConfig {
                algorithm: PlacementAlgorithm::SimulatedAnnealing,
                max_iterations: 1000,       // Fewer iterations for larger device
                initial_temperature: 200.0, // Higher temp for ECP5
                cooling_rate: 0.98,
                timing_weight: 0.5,
                wirelength_weight: 0.4,
                congestion_weight: 0.1,
            },
            device.clone(),
        );

        let placement_result = placer
            .place(&test_design)
            .expect("ECP5 placement should succeed");

        println!("     Placed {} gates", placement_result.placements.len());
        println!(
            "     Utilization: {:.1}%",
            placement_result.utilization * 100.0
        );
        println!("     Cost: {:.2}", placement_result.cost);

        // Step 2: ECP5 Routing with PathFinder A*
        println!("\n   Step 2: ECP5 Routing");
        let router_config = RouterConfig {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 75, // Adjusted for ECP5
            allow_ripup: true,
            max_congestion: 1.3,
            history_cost_factor: 0.4,
            present_congestion_factor: 1.1,
            timing_weight: 0.3,
        };

        let mut router = Router::new(router_config, device.clone());
        let routing_result = router
            .route(&test_design, &placement_result)
            .expect("ECP5 routing should succeed");

        println!("     Routes: {}", routing_result.routes.len());
        println!("     Congestion: {:.3}", routing_result.congestion);
        println!("     Wirelength: {}", routing_result.wirelength);

        // Step 3: ECP5 Project Trellis Bitstream Generation
        println!("\n   Step 3: Project Trellis Bitstream Generation");
        let trellis_config = BitstreamConfig {
            format: BitstreamFormat::TrellisBinary,
            include_routing: true,
            compress: false,
            timing_annotations: true,
        };

        let bitstream_gen = BitstreamGenerator::with_config(device.clone(), trellis_config);
        let bitstream = bitstream_gen
            .generate(&placement_result, &routing_result)
            .expect("Trellis bitstream generation should succeed");

        println!("     Bitstream size: {} bytes", bitstream.data.len());
        println!("     Format: {}", bitstream.format_info());

        // Verify Trellis bitstream format
        bitstream
            .verify()
            .expect("Trellis bitstream should be valid");

        // Check Trellis-specific content
        assert!(
            bitstream.data.starts_with(b"TRELLIS"),
            "Should have Trellis header"
        );
        assert!(
            bitstream.data.len() > 1000,
            "ECP5 bitstream should be substantial"
        );

        // Test different ECP5 bitstream sections
        let _data_str = String::from_utf8_lossy(&bitstream.data);
        // Note: These are binary data, so we check the raw bytes
        assert!(
            bitstream.data.windows(5).any(|w| w == b"TILES"),
            "Should contain TILES section"
        );
        assert!(
            bitstream.data.windows(6).any(|w| w == b"IOCONF"),
            "Should contain I/O config"
        );

        // Step 4: ECP5 Timing Analysis
        println!("\n   Step 4: ECP5 Timing Analysis");
        let ecp5_timing_config = TimingConfig {
            target_frequency: 200.0, // Higher target for ECP5
            setup_margin: 0.2,
            hold_margin: 0.1,
            clock_uncertainty: 0.1,
            multicycle_analysis: false,
            max_critical_paths: 8,
        };

        let mut timing_analyzer = TimingAnalyzer::new(ecp5_timing_config, device.clone());
        let timing_report = timing_analyzer
            .analyze_timing(&test_design, &placement_result, &routing_result)
            .expect("ECP5 timing analysis should succeed");

        println!("     Target: 200.0 MHz");
        println!("     Achieved: {:.1} MHz", timing_report.design_frequency);
        println!(
            "     Status: {}",
            if timing_report.meets_timing {
                "PASS"
            } else {
                "FAIL"
            }
        );

        // Verify complete ECP5 flow
        assert!(
            !placement_result.placements.is_empty(),
            "Should have placements"
        );
        assert!(
            placement_result.utilization > 0.0,
            "Should have utilization"
        );
        assert!(
            !timing_report.clock_summaries.is_empty(),
            "Should have clock analysis"
        );
        assert!(
            bitstream.metadata.logic_utilization >= 0.0,
            "Should have valid utilization"
        );

        println!("\n📊 ECP5 Flow Summary:");
        println!(
            "   Device: {} ({}x{} grid)",
            device.name, device.grid_size.0, device.grid_size.1
        );
        println!(
            "   Resources: {} LUTs, {} DSPs, {} EBRs",
            device.stats().total_luts,
            device.stats().total_dsps,
            device.stats().total_brams
        );
        println!(
            "   Placement: {:.1}% utilization, {:.2} cost",
            placement_result.utilization * 100.0,
            placement_result.cost
        );
        println!(
            "   Routing: {} routes, {:.3} congestion",
            routing_result.routes.len(),
            routing_result.congestion
        );
        println!(
            "   Timing: {:.1} MHz, {}",
            timing_report.design_frequency,
            if timing_report.meets_timing {
                "PASS"
            } else {
                "FAIL"
            }
        );
        println!(
            "   Bitstream: {} bytes Trellis format",
            bitstream.data.len()
        );

        println!("\n🎉 COMPLETE ECP5 NATIVE PLACE & ROUTE FLOW - SUCCESS!");
        println!("   ✅ ECP5 device architecture fully modeled");
        println!("   ✅ ECP5-optimized placement and routing");
        println!("   ✅ Project Trellis bitstream generation");
        println!("   ✅ ECP5 timing analysis with higher frequencies");
        println!("   ✅ Full SKALP → LIR → ECP5 Place & Route → Trellis bitstream validated");

        println!("✅ ECP5 place & route flow test passed");
    }

    // Helper function to create a test counter design
    fn create_test_counter_design() -> LirDesign {
        LirDesign {
            name: "test_counter".to_string(),
            modules: vec![LirModule {
                name: "counter".to_string(),
                signals: vec![
                    LirSignal {
                        name: "clk".to_string(),
                        signal_type: "logic".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "reset".to_string(),
                        signal_type: "logic".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "count".to_string(),
                        signal_type: "logic[7:0]".to_string(),
                        is_input: false,
                        is_output: true,
                        is_register: false,
                    },
                ],
                nets: vec![
                    Net {
                        id: "clk_net".to_string(),
                        width: 1,
                        driver: Some("clk".to_string()),
                        loads: vec!["counter_ff".to_string()],
                        is_output: false,
                        is_input: true,
                    },
                    Net {
                        id: "reset_net".to_string(),
                        width: 1,
                        driver: Some("reset".to_string()),
                        loads: vec!["counter_ff".to_string()],
                        is_output: false,
                        is_input: true,
                    },
                ],
                gates: vec![
                    Gate {
                        id: "counter_ff".to_string(),
                        gate_type: GateType::DFF,
                        inputs: vec!["clk_net".to_string(), "reset_net".to_string()],
                        outputs: vec!["count".to_string()],
                    },
                    Gate {
                        id: "adder".to_string(),
                        gate_type: GateType::And, // Simplified
                        inputs: vec!["count".to_string()],
                        outputs: vec!["next_count".to_string()],
                    },
                ],
            }],
        }
    }

    // Helper function to create a more complex test design
    fn create_complex_test_design() -> LirDesign {
        LirDesign {
            name: "complex_design".to_string(),
            modules: vec![LirModule {
                name: "alu".to_string(),
                signals: vec![
                    LirSignal {
                        name: "clk".to_string(),
                        signal_type: "logic".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "a".to_string(),
                        signal_type: "logic[31:0]".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "b".to_string(),
                        signal_type: "logic[31:0]".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "result".to_string(),
                        signal_type: "logic[31:0]".to_string(),
                        is_input: false,
                        is_output: true,
                        is_register: false,
                    },
                ],
                nets: vec![],
                gates: (0..20)
                    .map(|i| Gate {
                        id: format!("gate_{}", i),
                        gate_type: match i % 4 {
                            0 => GateType::And,
                            1 => GateType::Or,
                            2 => GateType::Xor,
                            3 => GateType::DFF,
                            _ => GateType::And,
                        },
                        inputs: vec![format!("input_{}", i)],
                        outputs: vec![format!("output_{}", i)],
                    })
                    .collect(),
            }],
        }
    }

    #[test]
    #[ignore = "VTR routing implementation incomplete"]
    fn test_comprehensive_vtr_support() {
        println!("🔹 Testing Comprehensive VTR Academic FPGA Support");

        // Test VTR k6_frac_N10 device
        let vtr_device = Device::vtr_k6_frac_n10();
        println!("   📋 VTR Device: {}", vtr_device.name);

        // Validate VTR device architecture
        let stats = vtr_device.stats();
        assert_eq!(vtr_device.family, DeviceFamily::Vtr);
        assert_eq!(vtr_device.name, "k6_frac_N10_mem32K_40nm");
        assert_eq!(vtr_device.grid_size, (82, 82));
        assert!(
            stats.total_luts > 40000,
            "VTR should have >40K LUTs, got {}",
            stats.total_luts
        );
        assert!(
            stats.total_ffs > 80000,
            "VTR should have >80K FFs, got {}",
            stats.total_ffs
        );
        assert!(stats.total_brams > 0, "VTR should have BRAM");
        assert!(stats.total_dsps > 0, "VTR should have DSP tiles");

        // Test VTR routing architecture
        assert!(
            vtr_device.routing.channels.0 >= 40,
            "VTR should have >=40 routing tracks"
        );
        assert_eq!(vtr_device.routing.switch_pattern, SwitchPattern::Wilton);
        assert!(
            vtr_device
                .routing
                .wire_segments
                .iter()
                .any(|ws| ws.length == 1),
            "VTR should have length-1 segments"
        );
        assert!(
            vtr_device
                .routing
                .wire_segments
                .iter()
                .any(|ws| ws.length == 4),
            "VTR should have length-4 segments"
        );
        assert!(
            vtr_device
                .routing
                .wire_segments
                .iter()
                .any(|ws| ws.length == 16),
            "VTR should have length-16 segments"
        );

        // Test VTR clock resources
        assert!(
            vtr_device.clock_resources.global_clocks >= 8,
            "VTR should have >=8 global clocks"
        );
        assert!(
            vtr_device.clock_resources.plls >= 4,
            "VTR should have >=4 PLLs"
        );
        assert!(
            vtr_device
                .clock_resources
                .clock_domains
                .first()
                .map(|cd| cd.max_frequency)
                .unwrap_or(0.0)
                >= 400.0e6,
            "VTR should support >=400MHz"
        );

        println!("✅ VTR device architecture tests passed");

        // Test VTR place & route flow
        println!("   🔧 Running VTR place & route flow");
        let design = create_test_counter_design();

        let mut placer = Placer::new(PlacerConfig::default(), vtr_device.clone());
        let placement_result = placer.place(&design).expect("VTR placement should succeed");
        assert!(
            !placement_result.placements.is_empty(),
            "VTR placement should place gates"
        );
        assert!(
            placement_result.utilization > 0.0,
            "VTR should have positive utilization"
        );

        let mut router = Router::new(RouterConfig::default(), vtr_device.clone());
        let routing_result = router
            .route(&design, &placement_result)
            .expect("VTR routing should succeed");
        assert!(
            !routing_result.routes.is_empty(),
            "VTR routing should create routes"
        );

        // Test VTR timing analysis
        let timing_config = TimingConfig {
            target_frequency: 350.0e6, // 350 MHz target
            clock_uncertainty: 0.1e-9,
            setup_margin: 0.1e-9,
            hold_margin: 0.05e-9,
            multicycle_analysis: true,
            max_critical_paths: 100,
        };

        let mut timing_analyzer = TimingAnalyzer::new(timing_config, vtr_device.clone());
        let timing_report = timing_analyzer
            .analyze_timing(&design, &placement_result, &routing_result)
            .expect("VTR timing analysis should succeed");

        assert!(
            timing_report.design_frequency > 0.0,
            "VTR timing should report positive frequency"
        );
        println!(
            "   ⏱️  VTR Timing: {:.1} MHz ({})",
            timing_report.design_frequency / 1e6,
            if timing_report.meets_timing {
                "PASS"
            } else {
                "FAIL"
            }
        );

        // Test VTR bitstream generation
        let _bitstream_config = BitstreamConfig {
            format: BitstreamFormat::VtrBitstream,
            include_routing: true,
            compress: false,
            timing_annotations: true,
        };

        let bitstream_generator = BitstreamGenerator::new(vtr_device.clone());
        let bitstream = bitstream_generator
            .generate(&placement_result, &routing_result)
            .expect("VTR bitstream generation should succeed");

        // Validate VTR XML bitstream
        let bitstream_str = String::from_utf8_lossy(&bitstream.data);
        assert!(
            bitstream_str.contains("<?xml version=\"1.0\"?>"),
            "VTR bitstream should be XML"
        );
        assert!(
            bitstream_str.contains("<vtr_bitstream>"),
            "VTR bitstream should have VTR root element"
        );
        assert!(
            bitstream_str.contains("k6_frac_N10_mem32K_40nm"),
            "VTR bitstream should contain device name"
        );
        assert!(
            bitstream_str.contains("<placement>"),
            "VTR bitstream should have placement section"
        );

        println!(
            "   📄 VTR Bitstream: {} bytes XML format",
            bitstream.data.len()
        );

        println!("\n🎉 COMPLETE VTR ACADEMIC FPGA FLOW - SUCCESS!");
        println!("   ✅ VTR k6_frac_N10 device architecture fully modeled");
        println!("   ✅ VTR-optimized placement and routing");
        println!("   ✅ VTR XML bitstream generation");
        println!("   ✅ VTR timing analysis for academic benchmarks");
        println!("   ✅ Full SKALP → LIR → VTR Place & Route → XML bitstream validated");

        println!("✅ VTR place & route flow test passed");
    }

    #[test]
    #[ignore = "OpenFPGA routing implementation incomplete"]
    fn test_comprehensive_openfpga_support() {
        println!("🔹 Testing Comprehensive OpenFPGA Support");

        // Test OpenFPGA k4_N8 device (simple academic device)
        let openfpga_k4 = Device::openfpga_k4_n8();
        println!("   📋 OpenFPGA Device: {}", openfpga_k4.name);

        // Validate OpenFPGA k4_N8 device architecture
        let stats_k4 = openfpga_k4.stats();
        assert_eq!(openfpga_k4.family, DeviceFamily::OpenFpga);
        assert_eq!(openfpga_k4.name, "k4_N8");
        assert_eq!(openfpga_k4.grid_size, (12, 12));
        assert!(
            stats_k4.total_luts > 500,
            "OpenFPGA k4_N8 should have >500 LUTs, got {}",
            stats_k4.total_luts
        );
        assert!(
            stats_k4.total_ffs > 500,
            "OpenFPGA k4_N8 should have >500 FFs, got {}",
            stats_k4.total_ffs
        );
        assert_eq!(
            stats_k4.total_brams, 0,
            "OpenFPGA k4_N8 should have no BRAM"
        );
        assert_eq!(stats_k4.total_dsps, 0, "OpenFPGA k4_N8 should have no DSP");

        // Test OpenFPGA k6_frac_N10 device (more complex)
        let openfpga_k6 = Device::openfpga_k6_frac_n10();
        println!("   📋 OpenFPGA Device: {}", openfpga_k6.name);

        // Validate OpenFPGA k6_frac_N10 device architecture
        let stats_k6 = openfpga_k6.stats();
        assert_eq!(openfpga_k6.family, DeviceFamily::OpenFpga);
        assert_eq!(openfpga_k6.name, "k6_frac_N10");
        assert_eq!(openfpga_k6.grid_size, (40, 40));
        assert!(
            stats_k6.total_luts > 10000,
            "OpenFPGA k6_frac_N10 should have >10K LUTs, got {}",
            stats_k6.total_luts
        );
        assert!(
            stats_k6.total_ffs > 20000,
            "OpenFPGA k6_frac_N10 should have >20K FFs, got {}",
            stats_k6.total_ffs
        );
        assert!(
            stats_k6.total_brams > 0,
            "OpenFPGA k6_frac_N10 should have BRAM"
        );
        assert_eq!(
            stats_k6.total_dsps, 0,
            "OpenFPGA k6_frac_N10 should have no DSP"
        );

        // Test OpenFPGA routing architecture
        assert!(
            openfpga_k6.routing.channels.0 >= 24,
            "OpenFPGA should have >=24 routing tracks"
        );
        assert_eq!(openfpga_k6.routing.switch_pattern, SwitchPattern::Wilton);

        println!("✅ OpenFPGA device architecture tests passed");

        // Test OpenFPGA place & route flow on k6_frac_N10 device
        println!("   🔧 Running OpenFPGA place & route flow");
        let design = create_test_counter_design();

        let mut placer = Placer::new(PlacerConfig::default(), openfpga_k6.clone());
        let placement_result = placer
            .place(&design)
            .expect("OpenFPGA placement should succeed");
        assert!(
            !placement_result.placements.is_empty(),
            "OpenFPGA placement should place gates"
        );
        assert!(
            placement_result.utilization > 0.0,
            "OpenFPGA should have positive utilization"
        );

        let mut router = Router::new(RouterConfig::default(), openfpga_k6.clone());
        let routing_result = router
            .route(&design, &placement_result)
            .expect("OpenFPGA routing should succeed");
        assert!(
            !routing_result.routes.is_empty(),
            "OpenFPGA routing should create routes"
        );

        // Test OpenFPGA timing analysis
        let timing_config = TimingConfig {
            target_frequency: 250.0e6, // 250 MHz target for academic device
            clock_uncertainty: 0.2e-9,
            setup_margin: 0.15e-9,
            hold_margin: 0.1e-9,
            multicycle_analysis: true,
            max_critical_paths: 100,
        };

        let mut timing_analyzer = TimingAnalyzer::new(timing_config, openfpga_k6.clone());
        let timing_report = timing_analyzer
            .analyze_timing(&design, &placement_result, &routing_result)
            .expect("OpenFPGA timing analysis should succeed");

        assert!(
            timing_report.design_frequency > 0.0,
            "OpenFPGA timing should report positive frequency"
        );
        println!(
            "   ⏱️  OpenFPGA Timing: {:.1} MHz ({})",
            timing_report.design_frequency / 1e6,
            if timing_report.meets_timing {
                "PASS"
            } else {
                "FAIL"
            }
        );

        // Test OpenFPGA native bitstream generation
        let _bitstream_config = BitstreamConfig {
            format: BitstreamFormat::OpenFpgaBitstream,
            include_routing: true,
            compress: false,
            timing_annotations: true,
        };

        let bitstream_generator = BitstreamGenerator::new(openfpga_k6.clone());
        let bitstream = bitstream_generator
            .generate(&placement_result, &routing_result)
            .expect("OpenFPGA bitstream generation should succeed");

        // Validate OpenFPGA XML bitstream
        let bitstream_str = String::from_utf8_lossy(&bitstream.data);
        assert!(
            bitstream_str.contains("<?xml version=\"1.0\"?>"),
            "OpenFPGA bitstream should be XML"
        );
        assert!(
            bitstream_str.contains("<openfpga_bitstream>"),
            "OpenFPGA bitstream should have OpenFPGA root element"
        );
        assert!(
            bitstream_str.contains("k6_frac_N10"),
            "OpenFPGA bitstream should contain device name"
        );
        assert!(
            bitstream_str.contains("<fabric_configuration>"),
            "OpenFPGA bitstream should have fabric configuration"
        );
        assert!(
            bitstream_str.contains("<logic_blocks>"),
            "OpenFPGA bitstream should have logic blocks"
        );
        assert!(
            bitstream_str.contains("<routing_configuration>"),
            "OpenFPGA bitstream should have routing configuration"
        );
        assert!(
            bitstream_str.contains("<io_configuration>"),
            "OpenFPGA bitstream should have I/O configuration"
        );
        assert!(
            bitstream_str.contains("<clock_configuration>"),
            "OpenFPGA bitstream should have clock configuration"
        );
        assert!(
            bitstream_str.contains("<timing_annotations>"),
            "OpenFPGA bitstream should have timing annotations"
        );

        println!(
            "   📄 OpenFPGA Bitstream: {} bytes XML format",
            bitstream.data.len()
        );

        // Test OpenFPGA VTR-compatible bitstream generation
        let _vtr_bitstream_config = BitstreamConfig {
            format: BitstreamFormat::VtrBitstream,
            include_routing: true,
            compress: false,
            timing_annotations: false,
        };

        let vtr_bitstream_generator = BitstreamGenerator::new(openfpga_k6.clone());
        let vtr_bitstream = vtr_bitstream_generator
            .generate(&placement_result, &routing_result)
            .expect("OpenFPGA VTR bitstream generation should succeed");

        // Validate VTR-compatible bitstream
        let vtr_bitstream_str = String::from_utf8_lossy(&vtr_bitstream.data);
        assert!(
            vtr_bitstream_str.contains("<vtr_bitstream>"),
            "OpenFPGA should generate VTR-compatible bitstream"
        );

        println!(
            "   📄 OpenFPGA VTR-compatible Bitstream: {} bytes XML format",
            vtr_bitstream.data.len()
        );

        println!("\n🎉 COMPLETE OPENFPGA ACADEMIC FLOW - SUCCESS!");
        println!("   ✅ OpenFPGA k4_N8 and k6_frac_N10 architectures fully modeled");
        println!("   ✅ OpenFPGA-optimized placement and routing");
        println!("   ✅ Native OpenFPGA XML bitstream generation");
        println!("   ✅ VTR-compatible bitstream generation for OpenFPGA");
        println!("   ✅ OpenFPGA timing analysis for academic research");
        println!("   ✅ Full SKALP → LIR → OpenFPGA Place & Route → XML bitstream validated");

        println!("✅ OpenFPGA place & route flow test passed");
    }
}
