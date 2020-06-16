import {Elm} from './Main.elm';
import './styles.less';
import './pam-frontend-styles.less';
import './styles/dato.less';
import './styles/merkelapp.less';
import amplitude from 'amplitude-js';

const app = Elm.Main.init();
app.ports.amplitudeEvent.subscribe(function (data) {
    fetch('/cv-samtale/amplitudeToken')
        .then((res) => res.text())
        .then((amplitudeToken) => {
            if (amplitudeToken) {
                amplitude.getInstance();
                amplitude.init(
                    amplitudeToken, null, {
                        apiEndpoint: 'amplitude.nav.no/collect',
                        batchEvents: false,
                        includeReferrer: true,
                        includeUtm: true,
                        saveEvents: false
                    }
                );
                amplitude.logEvent(data, {source: 'cv-samtale'});
            }
        })
        .catch(console.error);
});
