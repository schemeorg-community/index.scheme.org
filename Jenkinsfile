pipeline {
    
    agent {
        label 'docker'
    }

    parameters {
        booleanParam(name: 'DEPLOY_STAGING', defaultValue: false, description: 'Deploy to index.staging.scheme.org')
        booleanParam(name: 'DEPLOY_PROD', defaultValue: false, description: 'Deploy to index.scheme.org')
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
                    image 'docker:20.10.24-cli'
                    args '-v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v /var/run/docker.sock:/var/run/docker.sock -u root'
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

        stage('Deploy staging') {
            agent {
                dockerfile {
                    filename './deploy/rsync.Dockerfile'
                    args '-u indexupdater'
                    reuseNode true
                }
            }
            when {
                expression {
                    return params.DEPLOY_STAGING
                }
            }
            steps {
                sshagent(credentials: ['index_staging_tuonela_ssh']) {
                    sh '''
                        rsync -e "ssh -o StrictHostKeyChecking=no" schemeindex.zip stag-index@tuonela.scheme.org:/staging/index/update/schemeindex.zip
                        ssh stag-index@tuonela.scheme.org 'cd ~ ; bash install-update.sh'
                    '''
                }
            }
        }

        stage('Deploy production') {
            agent {
                dockerfile {
                    filename './deploy/rsync.Dockerfile'
                    reuseNode true
                }
            }
            when {
                expression {
                    return params.DEPLOY_PROD
                }
            }
            steps {
                sshagent(credentials: ['index_tuonela_ssh']) {
                    sh '''
                        mkdir ~/.ssh
                        ssh-keyscan -t rsa tuonela.scheme.org >> ~/.ssh/known_hosts
                        rsync schemeindex.zip prod-index@tuonela.scheme.org:/production/index/update/schemeindex.zip
                        ssh prod-index@tuonela.scheme.org 'cd ~ ; bash install-update.sh'
                    '''
                }
            }
        }

    }

}
