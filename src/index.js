import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Main.embed(document.getElementById('root'));

registerServiceWorker();
//app.ports.newServiceWorkerAvailable.send(true);

app.ports.refreshPage.subscribe(function() {
    window.location.reload();
    //app.ports.newServiceWorkerAvailable.send(false);
});