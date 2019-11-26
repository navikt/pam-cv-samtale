const express = require('express');
const proxy = require('express-http-proxy');
const Bundler = require('parcel-bundler');
const path = require('path');

const server = express();


server.get('/cv-samtale/login', (req, res) => {
    if (req.query.redirect) {
        res.redirect(`http://localhost:1337/pam-cv-api/local/cookie?redirect=http://localhost:1234${req.query.redirect}`);
    } else {
        res.redirect('http://localhost:1337/pam-cv-api/local/cookie?redirect=http://localhost:1234');
    }
});

server.post('/cv-samtale/log', express.json(), (req, res) => {
    console.log({
        ...req.body,
        level: 'Error'
    });
    res.sendStatus(200);
});

server.use(
    ['/cv-samtale/api', '/cv/api'],
    proxy('http://localhost:1337', {
        proxyReqPathResolver: req => (
            req.originalUrl.replace(new RegExp('/cv(-samtale)?/api'), '/pam-cv-api')
        ),
        proxyErrorHandler: (err, res, next) => {
            if (err && err.code) {
                console.log({ level: 'Error', message: err.code });
            }
            next(err);
        }
    })
);

const entryFile = path.join(__dirname, process.env.ENTRY_FILE || './src/index.html');
const bundler = new Bundler(entryFile, {});
server.use(bundler.middleware());


const port = process.env.PORT || 1234;
server.listen(port, () => {
    console.log('Server listening on port', port);
});
