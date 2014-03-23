node bin/compile-boot.js > build/boot.js
browserify wat.js --standalone wat > build/wat.js
#browserify wat.js --standalone wat --exclude wat-parser.js --exclude jsparse.js > build/wat-noparser.js
