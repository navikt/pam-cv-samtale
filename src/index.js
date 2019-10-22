import { Elm } from './Main.elm';
import './styles.less';
import './pam-frontend-styles.less';

const app = Elm.Main.init();

window.onfocus = () => {
    app.ports.fromJavascriptToElm.send({ type: 'WINDOW_IN_FOCUS' });
};
