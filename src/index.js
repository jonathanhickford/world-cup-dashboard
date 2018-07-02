import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Main.embed(document.getElementById('root'));

window.newServiceWorkerAvailable = function() {
    app.ports.newServiceWorkerAvailable.send(true);
}

registerServiceWorker();
//app.ports.newServiceWorkerAvailable.send(true);

app.ports.refreshPage.subscribe(function() {
    window.location.reload();
    //app.ports.newServiceWorkerAvailable.send(false);
});

app.ports.scrollIntoView.subscribe( (elem) => {
     requestAnimationFrame( () => {
        const element = document.getElementById(elem);
        if (element == null) {
           throw new Error("Element not found: " + elem);
        } else {
            if (element.scrollIntoViewIfNeeded != null) {
              element.scrollIntoViewIfNeeded(false);
            } else {
           element.scrollIntoView();
            }
       }
    })
});