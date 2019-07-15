import * as express from 'express';
import { Request} from 'express';
import * as proxy from 'express-http-proxy';
import * as helmet from 'helmet';
import * as path from 'path';
import { RequestOptions } from 'http';

if (!process.env.PAM_CV_API_PROXY_KEY) {
    throw new Error("Miljøvariabel PAM_CV_API_PROXY_KEY er ikke satt");
}
if (!process.env.API_GATEWAY_HOST) {
    throw new Error("Miljøvariabel API_GATEWAY_HOST er ikke satt");
}

const MILJOVARIABLER = {
    API_GATEWAY_HOST: process.env.API_GATEWAY_HOST,
    PROXY_API_KEY: process.env.PAM_CV_API_PROXY_KEY
};

console.log(`API_GATEWAY_HOST: ${MILJOVARIABLER.API_GATEWAY_HOST}`);

const server = express();
server.use(express.json());

// security
server.disable('x-powered-by');
server.use(helmet());

// health checks
server.get('/cv-samtale/internal/isAlive', (req, res) => res.sendStatus(200));
server.get('/cv-samtale/internal/isReady', (req, res) => res.sendStatus(200));

server.post('/cv-samtale/log', (req, res) => {
    console.log(JSON.stringify({
        ...req.body,
        level: "Error"
    }));
    res.sendStatus(200);
});

const getCookie = (name: string, cookie: string) => {
    const re = new RegExp(`${name}=([^;]+)`);
    const match = re.exec(cookie);
    return match !== null ? match[1] : '';
};

server.use(
    '/cv-samtale/api',
    proxy('http://pam-cv', {
        // https: true,
        // proxyReqOptDecorator: (proxyReqOpts: RequestOptions, srcReq: Request)  => {
        //     // proxyReqOpts.headers['Cookie'] = srcReq.header('Cookie');
        //     proxyReqOpts.headers['X-XSRF-TOKEN'] = getCookie('XSRF-TOKEN', srcReq.header('Cookie'));
        //     proxyReqOpts.headers['x-nav-apiKey'] = MILJOVARIABLER.PROXY_API_KEY;
        //     return proxyReqOpts;
        // },
        // proxyReqPathResolver: (req: any) => {
        //     const p = req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/pam-cv-api/pam-cv-api');
        //     console.log(JSON.stringify({message: `New path: ${MILJOVARIABLER.API_GATEWAY_HOST}${p}`}))
        //     return p;
        // }
        proxyReqPathResolver: (req: any) => (
            req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/cv/api')
        ),
        parseReqBody: true,
        // proxyReqBodyDecorator: () => ''
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
