const path = require('path');

module.exports = function(grunt) {
    const caminho = path.join(__dirname, 'unittest');
    const caminhoBat = path.join(caminho, 'test.bat');
    const caminhoSQL = path.join(__dirname,'unittest', 'SQL');
    const caminhoBatSQL = path.join(caminhoSQL, 'test.bat');
    const caminhoBatSQLFPC = path.join(caminhoSQL, 'test_fpc.bat');    
    const caminhoORM = path.join(__dirname,'unittest', 'ORM');
    const caminhoBatORM = path.join(caminhoORM, 'test.bat');
    grunt.initConfig({
      commands: {
        test: {
          force: false,          
          cmd: [
            `cd "${caminho}"`,
            `test.bat`            
          ]
        }
      },
      bgShell: {
        server: {
          cmd: `${caminhoBat} "${caminho}"`,
          execOptions: caminho,
          execOpts: {
            stdio: 'inherit'
          }
        },
        sql: {
          cmd: `${caminhoBatSQL} "${caminhoSQL}"`,
          execOptions: caminhoSQL,
          execOpts: {
            stdio: 'inherit'
          }
        },
        sql_fpc: {
          cmd: `${caminhoBatSQLFPC} "${caminhoSQL}"`,
          execOptions: caminhoSQL,
          execOpts: {
            stdio: 'inherit'
          }
        },
        orm: {
          cmd: `${caminhoBatORM} "${caminhoORM}"`,
          execOptions: caminhoORM,
          execOpts: {
            stdio: 'inherit'
          }
        },
        stdout: true
      },
      watch: {
        server: {
          files: ['**/*.pas'],
          tasks: ['bgShell:server']
        },
        sql: {
          files: ['**/*.pas'],
          tasks: ['bgShell:sql']
        },
        sql_fpc: {
          files: ['*.pas', 'unittest/SQL/*.pas'],
          tasks: ['bgShell:sql_fpc']
        },
        orm: {
          files: ['**/*.pas'],
          tasks: ['bgShell:orm']
        }
      }
    });
  
    grunt.loadNpmTasks('grunt-commands');
    grunt.loadNpmTasks('grunt-bg-shell');
    grunt.loadNpmTasks('grunt-contrib-watch');
  
    grunt.registerTask('default', ['watch:server']);
    grunt.registerTask('test_sql_fpc', ['bgShell:sql_fpc']);
  };