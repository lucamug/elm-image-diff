(function(){
    var app = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: {
        localStorage: String(localStorage.getItem("elm")),
        locationHref: location.href.startsWith("blob") ? "https://example.com/" : location.href,
        posix: new Date().getTime(),
        fp: fp("")
    }

    });
    app.ports.pushUrl.subscribe(function(url) {
        if (location.href.startsWith("blob")) {
            app.ports.onUrlChange.send("https://example.com" + url);
        } else {
            history.pushState({}, "", url);
            app.ports.onUrlChange.send(location.href);
        }
    });
    app.ports.pushLocalStorage.subscribe(function(state) {
        localStorage.setItem("elm", state);
    });
    app.ports.requestDiff.subscribe(function([url1, url2, [x, y] ]) {
        requestAnimationFrame(function() {buildDiff(url1, url2, x, y)});
    });
    window.onpopstate = function() {
        app.ports.onUrlChange.send(location.href);
    };
    window.onstorage = function(event) {
        app.ports.onLocalStorageChange.send(event.newValue);
    };

    function addImageProcess(src) {
        return new Promise((resolve, reject) => {
            let img = new Image()
            img.onload = () => resolve(img)
            img.crossOrigin = 'anonymous';
            img.onerror = reject
            img.src = src
        })
    }

    function buildDiff(url1, url2, x, y) {
        
        var canvas1 = document.getElementById('canvas1');

        var canvas2 = document.getElementById('canvas2');

        var canvasDiff = document.getElementById('diff');

        if (canvas1 && canvas2 && canvasDiff ) {
            
            addImageProcess(url1)
                .then((i1) => {
                    addImageProcess(url2)
                        .then((i2) => {

                            context1 = canvas1.getContext('2d');
                            context1.drawImage(i1, 0, 0);

                            context2 = canvas2.getContext('2d');
                            context2.drawImage(i2, 0, 0);

                            contextDiff = canvasDiff.getContext('2d');

                            const imageData1 = context1.getImageData(0, 0, x, y);
                            const imageData2 = context2.getImageData(0, 0, x, y);
                            const imageDataDiff = contextDiff.createImageData(x, y);

                            // 
                            // Github colors
                            // 
                            // 254 219 223 Red dark
                            // 254 238 240 Red light
                            // 
                            // 
                            // 203 255 216 Green dark
                            // 229 255 235 Green light
                            // 

                            const options = 
                                // This conf is also in omni/cmd/visual-regression/04-detect-diff-screenshots.js
                                { includeAA: true
                                , threshold: 0.1
                                , alpha: 0.2
                                , aaColor: [250, 100, 100]
                                , diffColor: [99, 190, 122]
                                , diffColorAlt: [222, 126, 134]
                                , diffMask: false
                                };

                            pixelmatch(imageData1.data, imageData2.data, imageDataDiff.data, x, y, options);
                            contextDiff.putImageData(imageDataDiff, 0, 0);
                        });
                });
        } else {
            // Canvases are not ready
        }
    }

    function fp (return_as_array = true) {
      try {
        const browser_features = [];
        const browser = navigator;
        for (let key in browser) {
          let value = browser[key];
          if (typeof value === 'object') value = JSON.stringify(value);
          browser_features.push(`${key}:${value}`);
        }
        return return_as_array ? browser_features : browser_features.join(',');
      } catch (e) {
        return '';
      }
    };

})();
