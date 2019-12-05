const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');

module.exports = {
    entry: {
        "app": "./src/app.js"
    },
    output: {
        publicPath: '/'
    },
    mode: "development",
    devtool: "inline-source-map",
    devServer: {
        contentBase: "./dist",
        historyApiFallback: true,
        hot: true,
        port: 9292,
        headers: {
            "Access-Control-Allow-Origin": "*",
            "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
            "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization, api-key, api-version"
        }
    },
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    { loader: "elm-hot-webpack-loader" },
                    {
                        loader: "elm-webpack-loader"
                        , options: {
                            debug: process.env.NODE_ENV === 'development'
                        }
                    }
                ]
            },
            {
                test: /\.css$/,
                use: [
                    {
                        loader: MiniCssExtractPlugin.loader,
                        options: {
                            publicPath: "./src/css",
                            hmr: process.env.NODE_ENV === 'development',
                        }
                    },
                    'css-loader',
                    'postcss-loader'
                ]
            },
        ]
    },
    plugins: [
        new CleanWebpackPlugin(),
        new MiniCssExtractPlugin(),
        new HtmlWebpackPlugin()
    ]
};
