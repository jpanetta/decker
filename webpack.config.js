const path = require('path');

module.exports = {
  target: "web",
  entry: {
    decker: './src-support/decker.js',
    math: './src-support/math.js',
    plugins: './src-support/rplugins.js',
    classlist: './src-support/classlist.js',
    notes: './src-support/notes.js',
    menu: './src-support/menu.js',
    mathjax: './src-support/math.js'
  },
  output: {
    path: path.resolve(__dirname, 'resource', 'support'),
    filename: '[name].js'
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader"
        }
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\.scss$/,
        use: [
          "style-loader",
          "css-loader",
          "sass-loader"
        ]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [{
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
            outputPath: 'fonts/'
          }
        }]
      }
    ]
  }
};