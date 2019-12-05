import hljs from 'highlight.js';
import 'highlight.js/styles/github.css';
import { Elm } from './elm/Main.elm';
import './css/app.css';

window.hljs = hljs;

const apiConfigString = localStorage ? localStorage.getItem("api.config") : null;
const apiConfig = apiConfigString ? JSON.parse(apiConfigString) : null;

console.log(apiConfig);

const app = Elm.Main.init({
    flags: {
        enviroment: process.env.NODE_ENV,
        apiConfig: apiConfig
    }
});

app.ports.logError.subscribe((m) => console.error(m));

if (app.ports.log) {
    app.ports.log.subscribe((m) => console.log(m));
}

if (app.ports.alert) {
    app.ports.alert.subscribe((m) => window.alert(m));
}

app.ports.storeApiConfig.subscribe((config) => localStorage.setItem('api.config', JSON.stringify(config)));
app.ports.deleteApiConfig.subscribe(() => localStorage.removeItem('api.config'));
