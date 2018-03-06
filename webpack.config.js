const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');
const merge = require('webpack-merge');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HTMLWebpackPlugin = require('html-webpack-plugin');


const NPM_CMD = process.env.npm_lifecycle_event;


const TARGET = {
    'build-web': 'prod-ui',
    'web': 'development',
    'web-admin': 'dev-admin-ui',
    'web-delegation': 'dev-delegation-ui'
}[NPM_CMD] || NPM_CMD;


require('dotenv').config({
    systemvars: true,
    path: TARGET.slice(3) === 'dev' ? './.env-dev' : './.env-prod'
});


console.log("TARGET =", TARGET);


const defaultFilename = '[name]-[hash].js';


const _pureDist = '_pureDist';
const _dist = '_dist';
const outputPath = path.join(__dirname, _dist);
const CopyWebpackPluginConfig = new CopyWebpackPlugin([
    {from: './web/css', to: outputPath + '/css'},
    {from: './web/js', to: outputPath + '/js'},
    {from: './web/img', to: outputPath + '/img'},
    {from: './web/_redirects', to: outputPath + '/'}
]);


const pursCommonF = ({outputDir}) => ({
    module: {
        rules: [
            {
                test: /\.purs$/,
                exclude: /node_modules/,
                use: [
                    {
                        loader: "purs-loader",
                        query: {
                            warnings: false,  // disable warnings for build time
                            bundle: true,   // disable bundle in attempt to reduce build time
                            output: outputDir || 'output',
                            src: ["bower_components/purescript-*/src/**/*.purs", "pureSrc/**/*.purs"],
                            jobs: 1,
                        }
                    }
                ]
            },
        ]
    },
    node: {
        fs: 'empty'
    },
    externals: {
        sodium: 'sodium',
        'decimal.js': 'Decimal',
        'yargs': 'yargs', // disable to avoid failure if purs not bundled
    }
});


const buildAuditWeb = pursArgs => merge(pursCommonF(pursArgs || {}), {
    resolve: {
        extensions: [".purs", ".js", ".json", ".ts"]
    },
});


const common = {
    node: {
        fs: "empty"
    },
    entry: {
        "sv-little-gov": ['./web/index.js']
    },
    output: {
        path: path.join(__dirname, _dist),
        filename: defaultFilename,
    },
    module: {
        rules: [
            { test: /\.tsx?$/, loader: "ts-loader" },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]',
            },
            {
                test: /\.css$/,
                exclude: [
                    /elm-stuff/, /node_modules/
                ],
                loaders: ["css-loader"]
            }
        ]
    },
    plugins: [
        new webpack.EnvironmentPlugin(["MAIN_TITLE", "DEV", "DEMOC_HASH", "INDEX_ADDR"]),
        CopyWebpackPluginConfig,
        new HTMLWebpackPlugin({
            // using .ejs prevents other loaders causing errors
            template: 'web/index.ejs',
            // inject details of output file at end of body
            inject: 'body'
        })
    ],
    resolve: {
        extensions: [".js", ".json", ".ts"],
        modules: ['./node_modules'],
    },
    bail: true
};


const uiInjection = (env, config) => {
    const [entryFileName, extras] = {
        'dev-admin-ui': ['admin', {}],
        'development': ['index', buildAuditWeb({})],
        'dev-delegation-ui': ['delegation', {}],
        'prod-admin-ui': ['admin', {}],
        'prod-ui': ['index', buildAuditWeb({})],
        'prod-delegation-ui': ['delegation', {}]
    }[env];
    const filename = env.slice(0,3) === 'dev' ? '[name].js' : '[name]-[hash].js';
    const toRet = merge(config, extras, {
        plugins: [
            new HTMLWebpackPlugin({
                // using .ejs prevents other loaders causing errors
                template: `web/${entryFileName}.ejs`,
                // inject details of output file at end of body
                inject: 'body'
            })
        ],
        output: {
            path: path.join(__dirname, _dist),
            filename: filename,
        }
    });
    toRet.entry = {[entryFileName]: [`./web/${entryFileName}.js`]};
    return toRet;
};


const genConfig = () => {
    if (TARGET === 'development' || TARGET === 'dev-admin-ui' || TARGET === 'dev-delegation-ui') {
        console.log('Building for dev...');
        const toExport = merge(common, {
            // mode: "development",  // webpack v4
            plugins: [
                // Suggested for hot-loading
                new webpack.NamedModulesPlugin(),
                // Prevents compilation errors causing the hot loader to lose state
                new webpack.NoEmitOnErrorsPlugin(),
                new webpack.HotModuleReplacementPlugin()
            ],
            module: {
                rules: [
                    {
                        test: /\.elm$/,
                        exclude: [
                            /elm-stuff/, /node_modules/
                        ],
                        use: [
                            "elm-hot-loader",
                            "elm-webpack-loader?verbose=true&warn=false&maxInstances=1&debug=true" //&debug=true
                        ]
                    }
                ]
            },
            resolve: {
                extensions: [".purs", ".js", ".json", ".ts"]
            },
            devServer: {
                inline: true,
                stats: 'errors-only',
                contentBase: [path.join(__dirname)],
                hot: true,
                // For SPAs: serve index.html in place of 404 responses
                historyApiFallback: true,
                watchOptions: {
                    ignored: [/node_modules/, /bower_components/]
                }
            }
        });
        return uiInjection(TARGET, toExport)
    }

    if (TARGET === 'prod-ui' || TARGET === 'prod-admin-ui' || TARGET === 'prod-delegation-ui') {
        console.log('Building for prod...');
        const config = merge(common, buildAuditWeb({outputDir: ".cache/output"}), {
            // mode: "production",  // webpack v4
            plugins: [
                // Delete everything from output directory and report to user
                new CleanWebpackPlugin([_dist], {
                    root: __dirname,
                    exclude: [],
                    verbose: true,
                    dry: false
                }),
                CopyWebpackPluginConfig
            ],
            module: {
                rules: [
                    {
                        test: /\.elm$/,
                        exclude: [
                            /elm-stuff/, /node_modules/
                        ],
                        use: [
                            {
                                loader: "elm-webpack-loader",
                                query: {
                                    maxInstances: 1,
                                    verbose: true
                                }
                            }
                        ]
                    }
                ]
            },
            resolve: {
                extensions: [".purs", ".js", ".json", ".ts"]
            },
            bail: true
        });
        return uiInjection(TARGET, config);
    }

    if (TARGET === 'admin-dev') {
        console.log("Building Admin Dev");
        const toExport = merge(pursCommonF(), {
            entry: {
                "swarm-voting-admin": ['./pureSrc/SecureVote/Democs/SwarmMVP/Admin.purs']
            },
            resolve: {
                extensions: [".purs", ".js", ".json", ".ts"]
            }
        });

        toExport.module.rules[0].query.watch = true;

        return toExport;
    }


    if (TARGET === 'audit-web') {
        console.log("building audit web (test)");
        return merge(buildAuditWeb, {
            entry: {
                "swm-vote-audit": ['./pureSrc/SecureVote/Democs/SwarmMVP/AuditWeb.purs']
            },
            output: {
                path: path.join(__dirname, _dist, "js"),
                filename: defaultFilename
            }
        });
    }
};


module.exports = genConfig();
