const fs = require('fs');
const path = require('path');
const caminhoArquivoMemoryLeak = path.join(__dirname, 'Win32', 'Debug', 'MiniRESTSQLTest_MemoryManager_EventLog.txt');
if (fs.existsSync(caminhoArquivoMemoryLeak)){
  console.log('Existe memory leak');
  process.exit(1);
}