const path = require('path');

module.exports = function(grunt) {
    const caminho = path.join(__dirname, 'unittest');
    const caminhoBat = path.join(caminho, 'test.bat');
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
        test: {
          cmd: `${caminhoBat} "${caminho}"`,
          execOptions: caminho
        },
        stdout: true
      },
      watch: {
        files: ['**/*.pas'],
        tasks: ['bgShell:test']
      }
    });
  
    grunt.loadNpmTasks('grunt-commands');
    grunt.loadNpmTasks('grunt-bg-shell');
    grunt.loadNpmTasks('grunt-contrib-watch');
  
    grunt.registerTask('default', ['watch']);
  
  };