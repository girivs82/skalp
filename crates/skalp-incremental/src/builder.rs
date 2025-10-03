//! Build orchestration

use crate::{BuildResults, IncrementalResult};

pub struct Builder {
    config: crate::BuildConfig,
}

impl Builder {
    pub fn new(config: crate::BuildConfig) -> Self {
        Self { config }
    }

    pub async fn build(&self, target: &str) -> IncrementalResult<BuildResults> {
        let mut results = BuildResults::new(target.to_string());
        results.success = true;
        Ok(results)
    }
}
