const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const path = require('path');

const swarmOutputPath = path.join(__dirname, "_dist");
const CopyWebpackPluginConfig = new CopyWebpackPlugin([
  {from: './web/css', to: swarmOutputPath + '/css'},
  {from: './web/js', to: swarmOutputPath + '/js'}
]);

module.exports = {
  entry: {
    main: ['./web/index.js']  
  },
  output: {
    path: path.resolve(__dirname + '/_dist'),
    filename: "sv-swarm.js"
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader?verbose=true&warn=true',
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      }
    ]
  },
  plugins: [
    CopyWebpackPluginConfig
  ]
};
