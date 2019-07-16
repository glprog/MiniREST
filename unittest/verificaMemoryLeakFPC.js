const fs = require('fs');
const readline = require('readline');
let hasMemoryLeak = true;

const rl = readline.createInterface({
  input: fs.createReadStream('heap.trc'),
  crlfDelay: Infinity
});

rl.on('line', (line) => {
  if (line.indexOf('0 unfreed memory blocks : 0') > -1){
    hasMemoryLeak = false;
  }
});

rl.on('close', () => {
  if (hasMemoryLeak){
    console.log('Existe memory leak!');
    process.exit(1);
  }
})