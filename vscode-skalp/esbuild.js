const esbuild = require('esbuild');

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

async function main() {
    const ctx = await esbuild.context({
        entryPoints: ['src/extension.ts'],
        bundle: true,
        format: 'cjs',
        minify: production,
        sourcemap: !production,
        sourcesContent: false,
        platform: 'node',
        outfile: 'dist/extension.js',
        external: ['vscode'],
        logLevel: 'silent',
        plugins: [
            /* eslint plugin for warnings (optional) */
            {
                name: 'esbuild-problem-reporter',
                setup(build) {
                    build.onEnd(result => {
                        for (const msg of result.errors) {
                            console.error(`  ERROR: ${msg.text}`);
                        }
                        for (const msg of result.warnings) {
                            console.warn(`  WARN: ${msg.text}`);
                        }
                        if (result.errors.length === 0) {
                            console.log('[esbuild] build finished');
                        }
                    });
                },
            },
        ],
    });

    if (watch) {
        await ctx.watch();
        console.log('[esbuild] watching...');
    } else {
        await ctx.rebuild();
        await ctx.dispose();
    }
}

main().catch(e => {
    console.error(e);
    process.exit(1);
});
