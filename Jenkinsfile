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
                    args "-u root"
                    reuseNode true
                }
            }
            steps {
                sh '''
                    docker build -f ./build/Dockerfile . -t scheme-index:latest
                    docker create --name dummy scheme-index:latest
                    docker cp dummy:/schemeindex.zip ./schemeindex.zip
                    docker rm -f dummy
                '''
            }
        }

        stage('Deploy') {
            when {
                branch 'jenkins-build'
            }
            agent {
                docker {
                    image 'python:3.9.20'
                    reuseNode true
                }
            }
            steps {
                sh '''
                    pip install --include-deps ansible
                    ansible-playbook -v
                '''
            }
        }

    }

}
