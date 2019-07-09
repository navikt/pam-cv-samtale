import * as express from 'express';
import * as proxy from 'express-http-proxy';
import * as helmet from 'helmet';
import * as path from 'path';

if (!process.env.PAM_CV_API_PROXY_KEY) {
    throw new Error("Miljøvariabel PAM_CV_API_PROXY_KEY er ikke satt");
}

const MILJOVARIABLER = {
    PROXY_API_KEY: process.env.PAM_CV_API_PROXY_KEY
};

const server = express();

// security
server.disable('x-powered-by');
server.use(helmet());

// health checks
server.get('/cv-samtale/internal/isAlive', (req, res) => res.sendStatus(200));
server.get('/cv-samtale/internal/isReady', (req, res) => res.sendStatus(200));


server.use(
    '/cv-samtale/api',
    proxy('http://api-gateway', {
        proxyReqOptDecorator: (proxyReqOpts: any, srcReq: any) => ({
            ...proxyReqOpts,
            cookie: srcReq.headers.cookie,
            headers: {
                ...proxyReqOpts.headers,
                'x-nav-apiKey': MILJOVARIABLER.PROXY_API_KEY
            }
        }),
        proxyReqPathResolver: (req: any) => (
            req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/pam-cv-api/pam-cv-api')
        )
    })
);

server.use('/cv-samtale/static', express.static(path.resolve(__dirname, 'dist')));
server.use(
    '/cv-samtale*',
    (req: express.Request, res: express.Response) => {
        res.sendFile(path.resolve(__dirname, 'dist', 'index.html'));
    }
);

const port = process.env.PORT || 8080;
server.listen(port, () => {
    console.log('Server listening on port', port);
});
