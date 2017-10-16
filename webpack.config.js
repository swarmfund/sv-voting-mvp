const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');
const merge = require('webpack-merge');
const CleanWebpackPlugin = require('clean-webpack-plugin');
var HTMLWebpackPlugin = require('html-webpack-plugin');


var TARGET_ENV = process.env.npm_lifecycle_event === 'build-web' ? 'production' : 'development';
var filename = (TARGET_ENV == 'production') ? '[name]-[hash].js' : '[name].js';

const _dist = '_dist';
const swarmOutputPath = path.join(__dirname, _dist);
const CopyWebpackPluginConfig = new CopyWebpackPlugin([
  {from: './web/css', to: swarmOutputPath + '/css'},
  {from: './web/js', to: swarmOutputPath + '/js'}
]);


const common = {
    entry: {
        "sv-swarm": ['./web/index.js']
    },
    output: {
        path: path.join(__dirname, _dist),
        filename: filename
    },
    module: {
        rules: [
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                loader:  'file-loader?name=[name].[ext]',
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
    ]
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
    console.log(JSON.stringify(module.exports, null, 2))
}

if (TARGET_ENV === 'production') {
    console.log('Building for prod...');
    module.exports = merge(common, {
        plugins: [
            // Delete everything from output directory and report to user
            new CleanWebpackPlugin([_dist], {
                root:     __dirname,
                exclude:  [],
                verbose:  true,
                dry:      false
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
