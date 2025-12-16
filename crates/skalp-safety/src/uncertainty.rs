//! Uncertainty quantification for safety metrics
//!
//! This module provides Monte Carlo-based uncertainty analysis for safety metrics
//! per ISO 26262 requirements for confidence in metric values.
//!
//! # Key Features
//!
//! - Monte Carlo simulation for PMHF, SPFM, LF metrics
//! - Multiple FIT distribution types (LogNormal, Exponential, Weibull)
//! - Confidence interval calculation (90%, 95%, 99%)
//! - Sensitivity analysis to identify critical parameters
//! - Tornado chart data generation for visualization
//!
//! # ISO 26262 Reference
//!
//! ISO 26262-5 Section 8.4.6 requires confidence in FIT values and derived metrics.
//! This module enables expressing uncertainty in calculated metrics.

use rand::prelude::*;
use rand_distr::{Distribution, Exp, LogNormal, Normal};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Configuration for Monte Carlo uncertainty analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonteCarloConfig {
    /// Number of iterations (default: 10000)
    pub iterations: u32,
    /// Confidence level for intervals (default: 0.95)
    pub confidence_level: f64,
    /// Random seed for reproducibility (optional)
    pub seed: Option<u64>,
    /// Enable parallel execution
    pub parallel: bool,
    /// Convergence threshold (relative change)
    pub convergence_threshold: f64,
}

impl Default for MonteCarloConfig {
    fn default() -> Self {
        Self {
            iterations: 10000,
            confidence_level: 0.95,
            seed: None,
            parallel: false,
            convergence_threshold: 0.01,
        }
    }
}

impl MonteCarloConfig {
    /// Create configuration for quick analysis (fewer iterations)
    pub fn quick() -> Self {
        Self {
            iterations: 1000,
            confidence_level: 0.95,
            seed: None,
            parallel: false,
            convergence_threshold: 0.05,
        }
    }

    /// Create configuration for high-confidence analysis
    pub fn high_confidence() -> Self {
        Self {
            iterations: 100000,
            confidence_level: 0.99,
            seed: None,
            parallel: true,
            convergence_threshold: 0.001,
        }
    }

    /// Set seed for reproducibility
    pub fn with_seed(mut self, seed: u64) -> Self {
        self.seed = Some(seed);
        self
    }
}

/// FIT value with uncertainty distribution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UncertainFit {
    /// Nominal (expected) FIT value
    pub nominal: f64,
    /// Distribution describing uncertainty
    pub distribution: FitDistribution,
    /// Component or cell this FIT applies to
    pub source: String,
}

impl UncertainFit {
    /// Create a fixed FIT value (no uncertainty)
    pub fn fixed(nominal: f64, source: &str) -> Self {
        Self {
            nominal,
            distribution: FitDistribution::Fixed(nominal),
            source: source.to_string(),
        }
    }

    /// Create FIT with log-normal distribution
    ///
    /// Log-normal is commonly used for failure rates as it:
    /// - Is always positive
    /// - Has a long tail for rare high values
    /// - Represents multiplicative uncertainty well
    pub fn log_normal(nominal: f64, error_factor: f64, source: &str) -> Self {
        // Error factor (EF) at 90% confidence: P(X > nominal * EF) = 0.05
        // For log-normal: sigma = ln(EF) / 1.645
        let sigma = (error_factor.ln()) / 1.645;
        let mu = nominal.ln();
        Self {
            nominal,
            distribution: FitDistribution::LogNormal { mu, sigma },
            source: source.to_string(),
        }
    }

    /// Create FIT with exponential distribution
    pub fn exponential(nominal: f64, source: &str) -> Self {
        Self {
            nominal,
            distribution: FitDistribution::Exponential {
                lambda: 1.0 / nominal,
            },
            source: source.to_string(),
        }
    }

    /// Create FIT with Weibull distribution
    pub fn weibull(nominal: f64, shape: f64, source: &str) -> Self {
        // Scale parameter chosen so mean = nominal
        // For Weibull, mean = scale * Gamma(1 + 1/shape)
        let gamma_factor = gamma_function(1.0 + 1.0 / shape);
        let scale = nominal / gamma_factor;
        Self {
            nominal,
            distribution: FitDistribution::Weibull { shape, scale },
            source: source.to_string(),
        }
    }

    /// Sample a FIT value from the distribution
    pub fn sample<R: Rng>(&self, rng: &mut R) -> f64 {
        self.distribution.sample(rng)
    }
}

/// Distribution types for FIT uncertainty
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FitDistribution {
    /// Fixed value (no uncertainty)
    Fixed(f64),
    /// Log-normal distribution (common for failure rates)
    LogNormal { mu: f64, sigma: f64 },
    /// Exponential distribution
    Exponential { lambda: f64 },
    /// Weibull distribution
    Weibull { shape: f64, scale: f64 },
    /// Normal distribution (truncated at 0)
    Normal { mean: f64, std_dev: f64 },
    /// Uniform distribution
    Uniform { min: f64, max: f64 },
}

impl FitDistribution {
    /// Sample a value from the distribution
    pub fn sample<R: Rng>(&self, rng: &mut R) -> f64 {
        match self {
            FitDistribution::Fixed(v) => *v,
            FitDistribution::LogNormal { mu, sigma } => {
                if let Ok(dist) = LogNormal::new(*mu, *sigma) {
                    dist.sample(rng)
                } else {
                    mu.exp() // Fallback to mean
                }
            }
            FitDistribution::Exponential { lambda } => {
                if let Ok(dist) = Exp::new(*lambda) {
                    dist.sample(rng)
                } else {
                    1.0 / lambda // Fallback to mean
                }
            }
            FitDistribution::Weibull { shape, scale } => {
                // Using inverse transform sampling
                let u: f64 = rng.gen();
                scale * (-u.ln()).powf(1.0 / shape)
            }
            FitDistribution::Normal { mean, std_dev } => {
                if let Ok(dist) = Normal::new(*mean, *std_dev) {
                    dist.sample(rng).max(0.0) // Truncate at 0
                } else {
                    *mean
                }
            }
            FitDistribution::Uniform { min, max } => rng.gen_range(*min..*max),
        }
    }

    /// Get the expected (mean) value
    pub fn mean(&self) -> f64 {
        match self {
            FitDistribution::Fixed(v) => *v,
            FitDistribution::LogNormal { mu, sigma } => (mu + sigma * sigma / 2.0).exp(),
            FitDistribution::Exponential { lambda } => 1.0 / lambda,
            FitDistribution::Weibull { shape, scale } => scale * gamma_function(1.0 + 1.0 / shape),
            FitDistribution::Normal { mean, .. } => *mean,
            FitDistribution::Uniform { min, max } => (min + max) / 2.0,
        }
    }
}

/// Result of uncertainty analysis with confidence intervals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UncertainMetric {
    /// Mean value
    pub mean: f64,
    /// Standard deviation
    pub std_dev: f64,
    /// Median value
    pub median: f64,
    /// Lower bound of confidence interval
    pub ci_lower: f64,
    /// Upper bound of confidence interval
    pub ci_upper: f64,
    /// Confidence level used (e.g., 0.95)
    pub confidence_level: f64,
    /// Number of samples used
    pub samples: u32,
    /// Raw sample data (optional, for detailed analysis)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub raw_samples: Option<Vec<f64>>,
}

impl UncertainMetric {
    /// Create from samples
    pub fn from_samples(samples: &[f64], confidence_level: f64) -> Self {
        let n = samples.len();
        if n == 0 {
            return Self {
                mean: 0.0,
                std_dev: 0.0,
                median: 0.0,
                ci_lower: 0.0,
                ci_upper: 0.0,
                confidence_level,
                samples: 0,
                raw_samples: None,
            };
        }

        let mean = samples.iter().sum::<f64>() / n as f64;
        let variance = samples.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / (n - 1) as f64;
        let std_dev = variance.sqrt();

        let mut sorted = samples.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());

        #[allow(clippy::manual_is_multiple_of)]
        let median = if n % 2 == 0 {
            (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
        } else {
            sorted[n / 2]
        };

        let alpha = 1.0 - confidence_level;
        let lower_idx = ((alpha / 2.0) * n as f64).floor() as usize;
        let upper_idx = ((1.0 - alpha / 2.0) * n as f64).ceil() as usize - 1;

        let ci_lower = sorted[lower_idx.min(n - 1)];
        let ci_upper = sorted[upper_idx.min(n - 1)];

        Self {
            mean,
            std_dev,
            median,
            ci_lower,
            ci_upper,
            confidence_level,
            samples: n as u32,
            raw_samples: None,
        }
    }

    /// Check if the metric meets a target with given confidence
    pub fn meets_target(&self, target: f64, must_be_below: bool) -> bool {
        if must_be_below {
            // E.g., PMHF must be below target
            self.ci_upper <= target
        } else {
            // E.g., SPFM must be above target
            self.ci_lower >= target
        }
    }

    /// Get the coefficient of variation (CV)
    pub fn coefficient_of_variation(&self) -> f64 {
        if self.mean > 0.0 {
            self.std_dev / self.mean
        } else {
            0.0
        }
    }
}

/// Monte Carlo simulation results for safety metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonteCarloResults {
    /// PMHF uncertainty analysis
    pub pmhf: UncertainMetric,
    /// SPFM uncertainty analysis
    pub spfm: UncertainMetric,
    /// LF uncertainty analysis
    pub lf: UncertainMetric,
    /// Configuration used
    pub config: MonteCarloConfig,
    /// Convergence achieved
    pub converged: bool,
    /// Sensitivity analysis results
    pub sensitivity: Option<SensitivityAnalysis>,
}

/// Sensitivity analysis identifying critical parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SensitivityAnalysis {
    /// Sensitivity of each parameter (partial derivative approximation)
    pub parameter_sensitivities: HashMap<String, ParameterSensitivity>,
    /// Parameters ranked by importance
    pub critical_parameters: Vec<String>,
    /// Data for tornado chart visualization
    pub tornado_chart: TornadoChart,
}

/// Sensitivity of a single parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParameterSensitivity {
    /// Parameter name/identifier
    pub name: String,
    /// Nominal value
    pub nominal: f64,
    /// Sensitivity coefficient (∂metric/∂param)
    pub sensitivity: f64,
    /// Importance measure (normalized contribution to variance)
    pub importance: f64,
    /// Effect on PMHF when parameter is at lower bound
    pub pmhf_at_low: f64,
    /// Effect on PMHF when parameter is at upper bound
    pub pmhf_at_high: f64,
}

/// Data for tornado chart visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TornadoChart {
    /// Entries sorted by impact
    pub entries: Vec<TornadoEntry>,
    /// Baseline (nominal) PMHF
    pub baseline_pmhf: f64,
}

/// Single entry in tornado chart
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TornadoEntry {
    /// Parameter name
    pub parameter: String,
    /// PMHF when parameter is at low value
    pub pmhf_low: f64,
    /// PMHF when parameter is at high value
    pub pmhf_high: f64,
    /// Swing (high - low)
    pub swing: f64,
}

/// Run Monte Carlo uncertainty analysis on safety metrics
pub fn run_monte_carlo(
    fit_distributions: &HashMap<String, UncertainFit>,
    dc_values: &HashMap<String, f64>,
    config: &MonteCarloConfig,
) -> MonteCarloResults {
    let mut rng: Box<dyn RngCore> = if let Some(seed) = config.seed {
        Box::new(StdRng::seed_from_u64(seed))
    } else {
        Box::new(thread_rng())
    };

    let mut pmhf_samples = Vec::with_capacity(config.iterations as usize);
    let mut spfm_samples = Vec::with_capacity(config.iterations as usize);
    let mut lf_samples = Vec::with_capacity(config.iterations as usize);

    // Run Monte Carlo iterations
    for _ in 0..config.iterations {
        // Sample FIT values
        let sampled_fits: HashMap<String, f64> = fit_distributions
            .iter()
            .map(|(name, dist)| (name.clone(), dist.sample(&mut rng)))
            .collect();

        // Calculate metrics for this sample
        let (pmhf, spfm, lf) = calculate_metrics_from_fits(&sampled_fits, dc_values);

        pmhf_samples.push(pmhf);
        spfm_samples.push(spfm);
        lf_samples.push(lf);
    }

    // Compute statistics
    let pmhf = UncertainMetric::from_samples(&pmhf_samples, config.confidence_level);
    let spfm = UncertainMetric::from_samples(&spfm_samples, config.confidence_level);
    let lf = UncertainMetric::from_samples(&lf_samples, config.confidence_level);

    // Check convergence (coefficient of variation should be stable)
    let converged = pmhf.coefficient_of_variation() < config.convergence_threshold;

    // Run sensitivity analysis
    let sensitivity = Some(run_sensitivity_analysis(fit_distributions, dc_values));

    MonteCarloResults {
        pmhf,
        spfm,
        lf,
        config: config.clone(),
        converged,
        sensitivity,
    }
}

/// Calculate metrics from a set of FIT values
fn calculate_metrics_from_fits(
    fits: &HashMap<String, f64>,
    dc_values: &HashMap<String, f64>,
) -> (f64, f64, f64) {
    let total_fit: f64 = fits.values().sum();

    // PMHF = sum of (FIT * (1 - DC)) for dangerous failures
    // Simplified: assume all FIT contributes to dangerous unless has DC
    let mut pmhf = 0.0;
    for (name, fit) in fits {
        let dc = dc_values.get(name).copied().unwrap_or(0.0);
        pmhf += fit * (1.0 - dc);
    }

    // SPFM = 1 - (SPF_FIT / Total_FIT)
    // SPF_FIT = FIT of unprotected cells (DC = 0)
    let spf_fit: f64 = fits
        .iter()
        .filter(|(name, _)| dc_values.get(*name).copied().unwrap_or(0.0) < 0.01)
        .map(|(_, fit)| fit)
        .sum();
    let spfm = if total_fit > 0.0 {
        1.0 - (spf_fit / total_fit)
    } else {
        1.0
    };

    // LF = detected_MPF / total_MPF
    // Simplified: assume DC applies to latent fault detection too
    let total_mpf_fit = total_fit * 0.1; // Simplified: 10% are multi-point
    let detected_mpf: f64 = fits
        .iter()
        .map(|(name, fit)| {
            let dc = dc_values.get(name).copied().unwrap_or(0.0);
            fit * 0.1 * dc
        })
        .sum();
    let lf = if total_mpf_fit > 0.0 {
        detected_mpf / total_mpf_fit
    } else {
        1.0
    };

    (pmhf, spfm, lf)
}

/// Run sensitivity analysis to identify critical parameters
fn run_sensitivity_analysis(
    fit_distributions: &HashMap<String, UncertainFit>,
    dc_values: &HashMap<String, f64>,
) -> SensitivityAnalysis {
    let nominal_fits: HashMap<String, f64> = fit_distributions
        .iter()
        .map(|(name, dist)| (name.clone(), dist.nominal))
        .collect();

    let (baseline_pmhf, _, _) = calculate_metrics_from_fits(&nominal_fits, dc_values);

    let mut sensitivities = HashMap::new();
    let mut tornado_entries = Vec::new();

    // Vary each parameter by ±20% and measure effect
    let variation = 0.2;

    for (name, dist) in fit_distributions {
        let nominal = dist.nominal;
        let low = nominal * (1.0 - variation);
        let high = nominal * (1.0 + variation);

        // Calculate PMHF at low value
        let mut fits_low = nominal_fits.clone();
        fits_low.insert(name.clone(), low);
        let (pmhf_low, _, _) = calculate_metrics_from_fits(&fits_low, dc_values);

        // Calculate PMHF at high value
        let mut fits_high = nominal_fits.clone();
        fits_high.insert(name.clone(), high);
        let (pmhf_high, _, _) = calculate_metrics_from_fits(&fits_high, dc_values);

        // Sensitivity = (PMHF_high - PMHF_low) / (FIT_high - FIT_low)
        let sensitivity_coef = if (high - low).abs() > 1e-10 {
            (pmhf_high - pmhf_low) / (high - low)
        } else {
            0.0
        };

        let swing = (pmhf_high - pmhf_low).abs();

        sensitivities.insert(
            name.clone(),
            ParameterSensitivity {
                name: name.clone(),
                nominal,
                sensitivity: sensitivity_coef,
                importance: 0.0, // Will be normalized later
                pmhf_at_low: pmhf_low,
                pmhf_at_high: pmhf_high,
            },
        );

        tornado_entries.push(TornadoEntry {
            parameter: name.clone(),
            pmhf_low,
            pmhf_high,
            swing,
        });
    }

    // Sort tornado entries by swing
    tornado_entries.sort_by(|a, b| b.swing.partial_cmp(&a.swing).unwrap());

    // Calculate importance (normalized)
    let total_sensitivity: f64 = sensitivities.values().map(|s| s.sensitivity.abs()).sum();
    if total_sensitivity > 0.0 {
        for (_, sens) in sensitivities.iter_mut() {
            sens.importance = sens.sensitivity.abs() / total_sensitivity;
        }
    }

    // Get critical parameters (top 5 by importance)
    let mut sorted_params: Vec<_> = sensitivities
        .iter()
        .map(|(name, s)| (name.clone(), s.importance))
        .collect();
    sorted_params.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
    let critical_parameters: Vec<String> =
        sorted_params.into_iter().take(5).map(|(n, _)| n).collect();

    SensitivityAnalysis {
        parameter_sensitivities: sensitivities,
        critical_parameters,
        tornado_chart: TornadoChart {
            entries: tornado_entries,
            baseline_pmhf,
        },
    }
}

/// Format uncertainty analysis report
pub fn format_uncertainty_report(results: &MonteCarloResults) -> String {
    let mut output = String::new();

    output.push_str("=== Monte Carlo Uncertainty Analysis Report ===\n\n");

    output.push_str(&format!(
        "Configuration: {} iterations, {:.0}% confidence\n",
        results.config.iterations,
        results.config.confidence_level * 100.0
    ));
    output.push_str(&format!(
        "Convergence: {}\n\n",
        if results.converged {
            "YES"
        } else {
            "NO (may need more iterations)"
        }
    ));

    output.push_str("--- PMHF (Probabilistic Metric for HW Failures) ---\n");
    output.push_str(&format!("  Mean:     {:.2e} FIT\n", results.pmhf.mean));
    output.push_str(&format!("  Std Dev:  {:.2e} FIT\n", results.pmhf.std_dev));
    output.push_str(&format!("  Median:   {:.2e} FIT\n", results.pmhf.median));
    output.push_str(&format!(
        "  {:.0}% CI:  [{:.2e}, {:.2e}] FIT\n",
        results.pmhf.confidence_level * 100.0,
        results.pmhf.ci_lower,
        results.pmhf.ci_upper
    ));
    output.push_str(&format!(
        "  CV:       {:.1}%\n\n",
        results.pmhf.coefficient_of_variation() * 100.0
    ));

    output.push_str("--- SPFM (Single Point Fault Metric) ---\n");
    output.push_str(&format!("  Mean:     {:.2}%\n", results.spfm.mean * 100.0));
    output.push_str(&format!(
        "  Std Dev:  {:.2}%\n",
        results.spfm.std_dev * 100.0
    ));
    output.push_str(&format!(
        "  {:.0}% CI:  [{:.2}%, {:.2}%]\n\n",
        results.spfm.confidence_level * 100.0,
        results.spfm.ci_lower * 100.0,
        results.spfm.ci_upper * 100.0
    ));

    output.push_str("--- LF (Latent Fault Metric) ---\n");
    output.push_str(&format!("  Mean:     {:.2}%\n", results.lf.mean * 100.0));
    output.push_str(&format!("  Std Dev:  {:.2}%\n", results.lf.std_dev * 100.0));
    output.push_str(&format!(
        "  {:.0}% CI:  [{:.2}%, {:.2}%]\n\n",
        results.lf.confidence_level * 100.0,
        results.lf.ci_lower * 100.0,
        results.lf.ci_upper * 100.0
    ));

    if let Some(sensitivity) = &results.sensitivity {
        output.push_str("--- Sensitivity Analysis ---\n");
        output.push_str(&format!(
            "Baseline PMHF: {:.2e} FIT\n\n",
            sensitivity.tornado_chart.baseline_pmhf
        ));

        output.push_str("Top Critical Parameters:\n");
        for (i, param) in sensitivity.critical_parameters.iter().enumerate() {
            if let Some(sens) = sensitivity.parameter_sensitivities.get(param) {
                output.push_str(&format!(
                    "  {}. {} (importance: {:.1}%)\n",
                    i + 1,
                    param,
                    sens.importance * 100.0
                ));
            }
        }

        output.push_str("\nTornado Chart (top 5):\n");
        output.push_str(&format!(
            "{:<30} {:>12} {:>12} {:>12}\n",
            "Parameter", "Low PMHF", "High PMHF", "Swing"
        ));
        output.push_str(&format!("{}\n", "-".repeat(68)));
        for entry in sensitivity.tornado_chart.entries.iter().take(5) {
            output.push_str(&format!(
                "{:<30} {:>12.2e} {:>12.2e} {:>12.2e}\n",
                entry.parameter, entry.pmhf_low, entry.pmhf_high, entry.swing
            ));
        }
    }

    output
}

/// Gamma function approximation using Lanczos approximation
#[allow(clippy::excessive_precision)]
fn gamma_function(x: f64) -> f64 {
    if x <= 0.0 {
        return f64::INFINITY;
    }

    // Use Lanczos approximation for gamma function
    let g = 7;
    let coefficients = [
        0.99999999999980993,
        676.5203681218851,
        -1259.1392167224028,
        771.32342877765313,
        -176.61502916214059,
        12.507343278686905,
        -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7,
    ];

    if x < 0.5 {
        // Use reflection formula
        std::f64::consts::PI / ((std::f64::consts::PI * x).sin() * gamma_function(1.0 - x))
    } else {
        let x = x - 1.0;
        let mut a = coefficients[0];
        for (i, &c) in coefficients.iter().enumerate().skip(1) {
            a += c / (x + i as f64);
        }
        let t = x + g as f64 + 0.5;
        (2.0 * std::f64::consts::PI).sqrt() * t.powf(x + 0.5) * (-t).exp() * a
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixed_distribution() {
        let fit = UncertainFit::fixed(100.0, "test_cell");
        let mut rng = StdRng::seed_from_u64(42);

        for _ in 0..10 {
            assert!((fit.sample(&mut rng) - 100.0).abs() < f64::EPSILON);
        }
    }

    #[test]
    fn test_log_normal_distribution() {
        let fit = UncertainFit::log_normal(100.0, 3.0, "test_cell");
        let mut rng = StdRng::seed_from_u64(42);

        let samples: Vec<f64> = (0..10000).map(|_| fit.sample(&mut rng)).collect();
        let mean = samples.iter().sum::<f64>() / samples.len() as f64;

        // Mean should be close to expected (within 20%)
        assert!((mean - fit.distribution.mean()).abs() / fit.distribution.mean() < 0.2);
    }

    #[test]
    fn test_uncertain_metric_from_samples() {
        let samples = vec![10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0];
        let metric = UncertainMetric::from_samples(&samples, 0.95);

        assert!((metric.mean - 55.0).abs() < 0.1);
        assert!((metric.median - 55.0).abs() < 0.1);
        assert!(metric.ci_lower < metric.mean);
        assert!(metric.ci_upper > metric.mean);
    }

    #[test]
    fn test_monte_carlo_basic() {
        let mut fit_distributions = HashMap::new();
        fit_distributions.insert("cell1".to_string(), UncertainFit::fixed(50.0, "cell1"));
        fit_distributions.insert("cell2".to_string(), UncertainFit::fixed(30.0, "cell2"));

        let mut dc_values = HashMap::new();
        dc_values.insert("cell1".to_string(), 0.9);
        dc_values.insert("cell2".to_string(), 0.0);

        let config = MonteCarloConfig::quick().with_seed(42);
        let results = run_monte_carlo(&fit_distributions, &dc_values, &config);

        assert!(results.pmhf.mean > 0.0);
        assert!(results.spfm.mean > 0.0);
        assert!(results.spfm.mean <= 1.0);
    }

    #[test]
    fn test_monte_carlo_with_uncertainty() {
        let mut fit_distributions = HashMap::new();
        fit_distributions.insert(
            "cell1".to_string(),
            UncertainFit::log_normal(50.0, 2.0, "cell1"),
        );
        fit_distributions.insert(
            "cell2".to_string(),
            UncertainFit::log_normal(30.0, 2.0, "cell2"),
        );

        let dc_values = HashMap::new();

        let config = MonteCarloConfig::default().with_seed(42);
        let results = run_monte_carlo(&fit_distributions, &dc_values, &config);

        // With uncertainty, we should have non-zero std dev
        assert!(results.pmhf.std_dev > 0.0);
        // Confidence interval should be meaningful
        assert!(results.pmhf.ci_upper > results.pmhf.ci_lower);
    }

    #[test]
    fn test_sensitivity_analysis() {
        let mut fit_distributions = HashMap::new();
        fit_distributions.insert(
            "high_fit_cell".to_string(),
            UncertainFit::fixed(100.0, "high_fit_cell"),
        );
        fit_distributions.insert(
            "low_fit_cell".to_string(),
            UncertainFit::fixed(10.0, "low_fit_cell"),
        );

        let dc_values = HashMap::new();

        let sensitivity = run_sensitivity_analysis(&fit_distributions, &dc_values);

        // High FIT cell should be more critical
        assert!(sensitivity
            .critical_parameters
            .contains(&"high_fit_cell".to_string()));
    }

    #[test]
    fn test_meets_target() {
        // Use enough samples for meaningful confidence intervals
        let samples: Vec<f64> = (1..=100).map(|i| i as f64).collect();
        let metric = UncertainMetric::from_samples(&samples, 0.95);

        // Mean should be around 50.5, CI should be roughly [3, 98]

        // For must_be_below: check if ci_upper <= target
        assert!(metric.meets_target(150.0, true)); // 98 <= 150 => true
        assert!(!metric.meets_target(50.0, true)); // 98 <= 50 => false

        // For must_be_above: check if ci_lower >= target
        assert!(metric.meets_target(1.0, false)); // 3 >= 1 => true
        assert!(!metric.meets_target(10.0, false)); // 3 >= 10 => false
    }

    #[test]
    fn test_format_report() {
        let mut fit_distributions = HashMap::new();
        fit_distributions.insert("cell1".to_string(), UncertainFit::fixed(50.0, "cell1"));

        let dc_values = HashMap::new();
        let config = MonteCarloConfig::quick().with_seed(42);
        let results = run_monte_carlo(&fit_distributions, &dc_values, &config);

        let report = format_uncertainty_report(&results);

        assert!(report.contains("Monte Carlo Uncertainty Analysis"));
        assert!(report.contains("PMHF"));
        assert!(report.contains("SPFM"));
        assert!(report.contains("Sensitivity"));
    }

    #[test]
    fn test_weibull_distribution() {
        let fit = UncertainFit::weibull(100.0, 2.0, "test");
        let mut rng = StdRng::seed_from_u64(42);

        let samples: Vec<f64> = (0..10000).map(|_| fit.sample(&mut rng)).collect();
        let mean = samples.iter().sum::<f64>() / samples.len() as f64;

        // Mean should be close to nominal (within 30% for Weibull)
        assert!((mean - 100.0).abs() / 100.0 < 0.3);
    }

    #[test]
    fn test_gamma_function() {
        // Gamma(1) = 1
        assert!((gamma_function(1.0) - 1.0).abs() < 0.001);
        // Gamma(2) = 1
        assert!((gamma_function(2.0) - 1.0).abs() < 0.001);
        // Gamma(3) = 2
        assert!((gamma_function(3.0) - 2.0).abs() < 0.001);
        // Gamma(4) = 6
        assert!((gamma_function(4.0) - 6.0).abs() < 0.001);
    }
}
