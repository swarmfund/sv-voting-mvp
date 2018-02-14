const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');
const merge = require('webpack-merge');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HTMLWebpackPlugin = require('html-webpack-plugin');
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');
require('dotenv').config({systemvars: true});


var TARGET_ENV = function () {
    console.log('Generating TARGET_ENV');
    switch (process.env.npm_lifecycle_event) {
        case 'build-web':
            return 'production';
        case 'web':
            return 'development';

        case 'admin-dev':
            return 'admin-dev';
        case 'admin-prod':
            return 'admin-prod';

        case 'audit-dev':
            return 'audit-dev';
        case 'audit-prod':
            return 'audit-prod';

        default:
            console.warn("WARNING - Setting uncaught TARGET_ENV: ", process.env.npm_lifecycle_event);
            return process.env.npm_lifecycle_event;
    }
}();
var filename = (TARGET_ENV === 'production') ? '[name]-[hash].js' : '[name].js';


const _pureDist = '_pureDist';
const _dist = '_dist';
const swarmOutputPath = path.join(__dirname, _dist);
const CopyWebpackPluginConfig = new CopyWebpackPlugin([
    {from: './web/css', to: swarmOutputPath + '/css'},
    {from: './web/js', to: swarmOutputPath + '/js'},
    {from: './web/img', to: swarmOutputPath + '/img'},
    {from: './web/_redirects', to: swarmOutputPath + '/'}
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
        "sv-swarm": ['./web/index.js']
    },
    output: {
        path: path.join(__dirname, _dist),
        filename: filename,
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
        new webpack.EnvironmentPlugin(["MAIN_TITLE", "DEV"]),
        CopyWebpackPluginConfig,
        new HTMLWebpackPlugin({
            // using .ejs prevents other loaders causing errors
            template: 'web/index.ejs',
            // inject details of output file at end of body
            inject: 'body'
        })
    ],
    resolve: {
        extensions: [".js", ".json", ".ts"]
    },
    bail: true
};


if (TARGET_ENV === 'development') {
    console.log('Building for dev...');
    module.exports = merge(common, buildAuditWeb({}), {
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
                        "elm-webpack-loader?verbose=true&warn=false&maxInstances=2&debug=true"
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
}

if (TARGET_ENV === 'production') {
    console.log('Building for prod...');
    module.exports = merge(common, buildAuditWeb({outputDir: ".cache/output"}), {
        plugins: [
            // Delete everything from output directory and report to user
            new CleanWebpackPlugin([_dist], {
                root: __dirname,
                exclude: [],
                verbose: true,
                dry: false
            }),
            new UglifyJSPlugin(),
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


}

if (TARGET_ENV === 'admin-dev') {
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

    module.exports = toExport;
}


if (TARGET_ENV === 'audit-web') {
    console.log("building audit web (test)");
    module.exports = merge(buildAuditWeb, {
        entry: {
            "swm-vote-audit": ['./pureSrc/SecureVote/Democs/SwarmMVP/AuditWeb.purs']
        },
        output: {
            path: path.join(__dirname, _dist, "js"),
            filename: filename
        }
    });
}
