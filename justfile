build:
    spago build

bundle:
    spago bundle

dev:
    spago build --watch

format:
    purs-tidy format-in-place src/**/*.purs

lint:
    purs-tidy check src/**/*.purs

ci: lint build bundle

serve: bundle
    npx serve dist -l 10000

restart: bundle
    -pkill -f 'serve dist -l 10000'
    sleep 1
    npx serve dist -l 10000

clean:
    rm -rf output/ dist/index.js
