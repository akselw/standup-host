const express = require('express');
const Bundler = require('parcel-bundler');
const path = require('path');

const server = express();

const entryFile = path.join(__dirname, './src/index.html');
const bundler = new Bundler(entryFile, {});

server.post('/log', express.json(), (req, res) => {
    console.log({
        ...req.body,
        level: 'Error'
    });
    res.sendStatus(200);
});

server.use(bundler.middleware());

const port = process.env.PORT || 8080;
server.listen(port, () => {
    console.log('Server listening on port', port);
});
