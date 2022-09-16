const glob = require("glob");
const res = glob.sync(`./docs/screenshots/**/*.png`)
console.log(JSON.stringify(res, null, 4))