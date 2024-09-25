pipeline {
    
    agent {
        label 'docker'
    }
    
    stages {

        stage('Checkout') {
            steps {
                git changelog: true, branch: "${BRANCH_NAME}", url: 'https://github.com/schemeorg-community/index.scheme.org'
            }
        }

        stage('Build') {
            agent {
                docker {
                    image 'docker:cli'
                    args "-v /var/run/docker.sock:/var/run/docker.sock"
                    reuseNode true
                }
            }
            steps {
                sh '''
                    docker build -f ./build/Dockerfile .
                    docker create --name dummy scheme-index:latest
                    docker cp dummy:/schemeindex.zip /tmp/schemeindex.zip
                    docker rm -f dummy
                '''
            }
        }

    }

}
