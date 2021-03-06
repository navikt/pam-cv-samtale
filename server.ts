import * as express from 'express';
import { Request } from 'express';
import * as proxy from 'express-http-proxy';
import * as compression from 'compression';
import * as helmet from 'helmet';
import * as path from 'path';
import * as request from 'request';
import { RequestOptions } from 'http';
import { Response } from 'express';
import { NextFunction } from 'express';

const getEnvironmentVariable = (key) => {
    if (!process.env[key]) {
        throw new Error(`Miljøvariabel ${key} er ikke satt`);
    }
    return process.env[key];
};

const ENVIRONMENT_VARIABLES = {
    API_GATEWAY_HOST: getEnvironmentVariable('API_GATEWAY_HOST'),
    PROXY_API_KEY: getEnvironmentVariable('PAM_CV_API_PROXY_KEY'),
    LOGINSERVICE_URL: getEnvironmentVariable('LOGINSERVICE_URL'),
    LOGOUTSERVICE_URL: getEnvironmentVariable('LOGOUTSERVICE_URL'),
    AMPLITUDE_TOKEN: getEnvironmentVariable('AMPLITUDE_TOKEN')
};

console.log(`API_GATEWAY_HOST: ${ENVIRONMENT_VARIABLES.API_GATEWAY_HOST}`);

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


server.get('/cv-samtale/amplitudeToken', (req, res) => {
    res.send(ENVIRONMENT_VARIABLES.AMPLITUDE_TOKEN);
});

server.get('/cv-samtale/login', (req, res) => {
    if (req.query.redirect) {
        res.redirect(`${ENVIRONMENT_VARIABLES.LOGINSERVICE_URL}?level=Level3&redirect=https://${req.hostname}${req.query.redirect}`);
    } else {
        res.redirect(`${ENVIRONMENT_VARIABLES.LOGINSERVICE_URL}?level=Level3&redirect=https://${req.hostname}/cv-samtale`);
    }
});

server.get('/cv-samtale/logout', (req, res) => {
    res.redirect(ENVIRONMENT_VARIABLES.LOGOUTSERVICE_URL);
});

server.post('/cv-samtale/log', express.json(), (req, res) => {
    console.log(JSON.stringify({
        ...req.body,
        level: 'Error'
    }));
    res.sendStatus(200);
});

server.use(
    '/cv-samtale/api',
    proxy(ENVIRONMENT_VARIABLES.API_GATEWAY_HOST, {
        https: true,
        proxyReqOptDecorator: (proxyReqOpts: RequestOptions, srcReq: Request) => ({
            ...proxyReqOpts,
            headers: {
                ...proxyReqOpts.headers,
                'X-XSRF-TOKEN-ARBEIDSPLASSEN': getCookie('XSRF-TOKEN-ARBEIDSPLASSEN', srcReq.header('Cookie')),
                'x-nav-apiKey': ENVIRONMENT_VARIABLES.PROXY_API_KEY,
                'kilde': 'cv-samtale'
            }
        }),
        proxyReqPathResolver: (req: any) => (
            req.originalUrl.replace(new RegExp('/cv-samtale/api'), '/pam-cv-api/pam-cv-api')
        ),
        proxyErrorHandler: (err: any, res: Response, next: NextFunction) => {
            if (err && err.code) {
                if (err.code === 'ECONNRESET') {
                    console.log(JSON.stringify({
                        level: 'Info',
                        message: err.message ? `${err.code}: ${err.message}` : err.code
                    }));
                    return res.status(502).send('Fikk "ECONNRESET" på request til api-gateway');
                } else {
                    console.log(JSON.stringify({
                        level: 'Error',
                        message: err.message ? `${err.code}: ${err.message}` : err.code
                    }));
                }
            }
            next(err);
        }
    })
);

server.use('/cv-samtale/static', express.static(path.resolve(__dirname, 'dist')));

const loggMetrikkForCvValg = (kilde: string, req: express.Request) => {
    request({
        url: `${ENVIRONMENT_VARIABLES.API_GATEWAY_HOST}/pam-cv-api/pam-cv-api/rest/cv/registreringstype`,
        method: 'POST',
        headers: {
            'Cookie': req.header('Cookie') || '',
            'X-XSRF-TOKEN-ARBEIDSPLASSEN': getCookie('XSRF-TOKEN-ARBEIDSPLASSEN', req.header('Cookie')),
            'x-nav-apiKey': ENVIRONMENT_VARIABLES.PROXY_API_KEY,
            'kilde': kilde
        }
    }, (error, response) => {
        if (error) {
            console.log(JSON.stringify({ level: 'Error', message: 'server.ts: Metrikk-logging cv-valg feilet', error: error }));
        } else if (response && response.statusCode > 201) {
            console.log(JSON.stringify({
                level: response.statusCode === 401 || response.statusCode === 403 || response.statusCode === 406 ? 'Warning' : 'Error',
                message: `server.ts: Metrikk-logging cv-valg resulterte i status code: ${response.statusCode}`,
                body: response.body
            }))
        }
    });
};

const loggMetrikkForCvAvslutning = (utgang : string | undefined, seksjon: string | undefined) => {
    if (utgang && seksjon) {
        console.log(JSON.stringify({
            message: 'CV-samtale avsluttet',
            utgang,
            seksjon
        }));
    } else {
        console.log(JSON.stringify({
            level: 'Error',
            message: `server.ts: Goto-lenke brukt uten fullstending metrikk info. utgang=${utgang}, seksjon=${seksjon}`
        }));
    }
};

server.use(
    '/cv-valg/samtale*',
    (req: express.Request, res: express.Response) => {
        loggMetrikkForCvValg('cv-samtale', req);
        res.redirect(`https://${req.hostname}/cv-samtale`)
    }
);

server.use(
    '/cv-valg/skjema*',
    (req: express.Request, res: express.Response) => {
        loggMetrikkForCvValg('cv', req);
        res.redirect(`https://${req.hostname}/jobbprofil`)
    }
);

server.use(
    '/cv-valg*',
    (req: express.Request, res: express.Response) => {
        res.sendFile(path.resolve(__dirname, 'dist', 'inngang.html'));
    }
);


server.use(
    '/cv-samtale/goto/forsiden*',
    (req: express.Request, res: express.Response) => {
        const query = Array.isArray(req.query.seksjon) ? `${req.query.seksjon[0]}` : `${req.query.seksjon}`;
        loggMetrikkForCvAvslutning('arbeidsplassen-logo', query);
        res.redirect(`https://${req.hostname}/`);
    }
);

server.use(
    '/cv-samtale/goto/forhandsvis*',
    (req: express.Request, res: express.Response) => {
        const utgang = Array.isArray(req.query.utgang) ? `${req.query.utgang[0]}` : `${req.query.utgang}`;
        const query = Array.isArray(req.query.seksjon) ? `${req.query.seksjon[0]}` : `${req.query.seksjon}`;
        loggMetrikkForCvAvslutning(utgang, query);
        res.redirect(`https://${req.hostname}/forhandsvis`);
    }
);

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
