import * as express from 'express';
import { Request} from 'express';
import * as proxy from 'express-http-proxy';
import * as compression from 'compression';
import * as helmet from 'helmet';
import * as path from 'path';
import { RequestOptions } from 'http';
import { Response } from 'express';
import { NextFunction } from 'express';

const getMiljovariabel = (key) => {
    if (!process.env[key]) {
        throw new Error(`Miljøvariabel ${key} er ikke satt`);
    }
    return process.env[key];
};

const MILJOVARIABLER = {
    API_GATEWAY_HOST: getMiljovariabel('API_GATEWAY_HOST'),
    PROXY_API_KEY: getMiljovariabel('PAM_CV_API_PROXY_KEY'),
    LOGINSERVICE_URL: getMiljovariabel('LOGINSERVICE_URL'),
    LOGOUTSERVICE_URL: getMiljovariabel('LOGOUTSERVICE_URL')
};

console.log(`API_GATEWAY_HOST: ${MILJOVARIABLER.API_GATEWAY_HOST}`);

const server = express();

server.use(compression());

// security
server.disable('x-powered-by');
server.use(helmet());

// health checks
server.get('/cv-samtale/internal/isAlive', (req, res) => res.sendStatus(200));
server.get('/cv-samtale/internal/isReady', (req, res) => res.sendStatus(200));

const getCookie = (name: string, cookie: string) => {
    const re = new RegExp(`${name}=([^;]+)`);
    const match = re.exec(cookie);
    return match !== null ? match[1] : '';
};

server.get('/cv-samtale/login', (req, res) => {
    if (req.query.redirect) {
        res.redirect(`${MILJOVARIABLER.LOGINSERVICE_URL}?level=Level3&redirect=https://${req.hostname}${req.query.redirect}`);
    } else {
        res.redirect(`${MILJOVARIABLER.LOGINSERVICE_URL}?level=Level3&redirect=https://${req.hostname}/cv-samtale`);
    }
});

server.get('/cv-samtale/logout', (req, res) => {
    res.redirect(MILJOVARIABLER.LOGOUTSERVICE_URL);
});

server.post('/cv-samtale/log', express.json(), (req, res) => {
    console.log(JSON.stringify({
        ...req.body,
        level: "Error"
    }));
    res.sendStatus(200);
});

server.use(
    '/cv-samtale/api',
    proxy(MILJOVARIABLER.API_GATEWAY_HOST, {
        https: true,
        proxyReqOptDecorator: (proxyReqOpts: RequestOptions, srcReq: Request)  => ({
            ...proxyReqOpts,
            headers: {
                ...proxyReqOpts.headers,
                'Cookie': srcReq.header('Cookie'),
                'X-XSRF-TOKEN': getCookie('XSRF-TOKEN', srcReq.header('Cookie')),
                'x-nav-apiKey': MILJOVARIABLER.PROXY_API_KEY,
                'kilde': 'cv-samtale'
            }
        }),
        proxyReqPathResolver: (req: any) => (
            req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/pam-cv-api/pam-cv-api')
        ),
        proxyErrorHandler: (err: any, res: Response, next: NextFunction) => {
            if (err && err.code) {
                console.log(JSON.stringify({
                    level: "Error",
                    message: err.code
                }));
            }
            next(err);
        }
    })
);

server.use('/cv-samtale/static', express.static(path.resolve(__dirname, 'dist')));
server.use(
    '/cv-samtale*',
    (req: express.Request, res: express.Response) => {
        res.sendFile(path.resolve(__dirname, 'dist', 'index.html'));
    }
);

server.use(
    '/logget-inn*',
    (req: express.Request, res: express.Response) => {
        res.sendFile(path.resolve(__dirname, 'dist', 'lukkSiden.html'));
    }
);

const port = process.env.PORT || 8080;
server.listen(port, () => {
    console.log('Server listening on port', port);
});
