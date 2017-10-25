const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');
const merge = require('webpack-merge');
const CleanWebpackPlugin = require('clean-webpack-plugin');
var HTMLWebpackPlugin = require('html-webpack-plugin');


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
            return 'development'
    }
}();
var filename = (TARGET_ENV === 'production') ? '[name]-[hash].js' : '[name].js';


const _pureDist = '_pureDist';
const _dist = '_dist';
const swarmOutputPath = path.join(__dirname, _dist);
const CopyWebpackPluginConfig = new CopyWebpackPlugin([
    {from: './web/css', to: swarmOutputPath + '/css'},
    {from: './web/js', to: swarmOutputPath + '/js'},
    {from: './web/img', to: swarmOutputPath + '/img'}
]);


const pursCommonF = (mainModule) => ({
    module: {
        rules: [
            {
                test: /\.purs$/,
                loader: "purs-loader",
                exclude: /node_modules/,
                query: {
                    bundle: true,
                    pscBundleArgs: {
                        main: mainModule
                    },
                    src: ["bower_components/purescript-*/src/**/*.purs", "pureSrc/**/*.purs"]
                }
            }
        ]
    },
    output: {
        path: path.join(__dirname, _pureDist),
        filename: filename
    }
})


const common = {
    node: {
        fs: "empty"
    },
    entry: {
        "sv-swarm": ['./web/index.js']
    },
    output: {
        path: path.join(__dirname, _dist),
        filename: filename
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
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        // env: automatically determines the Babel plugins you need based on your supported environments
                        presets: ['env']
                    }
                }
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
    }
};


if (TARGET_ENV === 'development') {
    console.log('Building for dev...');
    module.exports = merge(common, {
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
                        "elm-webpack-loader?verbose=true&warn=true&debug=true&maxInstances=2"
                    ]
                }
            ]
        },
        devServer: {
            inline: true,
            stats: 'errors-only',
            contentBase: path.join(__dirname, "web"),
            hot: true,
            // For SPAs: serve index.html in place of 404 responses
            historyApiFallback: true
        }
    });
}

if (TARGET_ENV === 'production') {
    console.log('Building for prod...');
    module.exports = merge(common, {
        plugins: [
            // Delete everything from output directory and report to user
            new CleanWebpackPlugin([_dist], {
                root: __dirname,
                exclude: [],
                verbose: true,
                dry: false
            }),
            CopyWebpackPluginConfig,
            // TODO update to version that handles =>
            new webpack.optimize.UglifyJsPlugin()
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
                            loader: "elm-webpack-loader"
                        }
                    ]
                }
            ]
        }
    });


}

if (TARGET_ENV === 'admin-dev') {
    console.log("Building Admin Dev");
    const toExport = merge(pursCommonF("SecureVote.Democs.SwarmMVP.Admin"), {
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
