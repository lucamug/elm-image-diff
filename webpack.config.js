// Generated using webpack-cli https://github.com/webpack/webpack-cli

const path = require('path');

// const isProduction = process.env.NODE_ENV == 'production';
const isProduction = true;

const config = {
    target: "node",
    entry: './image-processing/all.js',
    output: {
        path: path.resolve(__dirname, 'build-ip'),
        filename: 'ip.js'
    },
    plugins: [
        // Add your plugins here
        // Learn more about plugins from https://webpack.js.org/configuration/plugins/
    ],
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/i,
                loader: 'babel-loader',
            }
            // Add your rules for custom modules here
            // Learn more about loaders from https://webpack.js.org/loaders/
        ],
    },
};

module.exports = () => {
    if (isProduction) {
        config.mode = 'production';
        
        
    } else {
        config.mode = 'development';
    }
    return config;
};
