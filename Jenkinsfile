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
            agent {
                dockerfile {
                    filename './deploy/ansible.Dockerfile'
                }
            }
            when {
                branch 'jenkins-build'
            }
            steps {
                dir('deploy') {
                    sh 'pip install ansible'
                    sshagent(credentials: ['index_scheme_org_ssh']) {
                        sh '''
                            ssh-keyscan -t rsa index.scheme.org >> ~/.ssh/known_hosts
                            ansible-playbook -i hosts deploy.yml -e content_zip_file=../schemeindex.zip
                        '''
                    }
                }
            }
        }

    }

}
