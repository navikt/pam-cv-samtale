const express = require('express');
const proxy = require('express-http-proxy');
const Bundler  = require('parcel-bundler');
const path  = require('path');

const server = express();
server.use(express.json())

const entryFile = path.join(__dirname, './src/index.html');
const bundler = new Bundler(entryFile, {});

server.post('/cv-samtale/log', (req, res) => {
    console.log({
        ...req.body,
        level: "Error"
    });
    res.sendStatus(200);
});

server.use(
    '/cv-samtale/api',
    proxy('http://localhost:1337', {
        proxyReqPathResolver: req => (
            req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/pam-cv-api')
        )
    })
);

server.use(bundler.middleware());


const port = process.env.PORT || 1234;
server.listen(port, () => {
    console.log('Server listening on port', port);
});
