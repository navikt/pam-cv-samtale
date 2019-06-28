// Obtain ephemeral "installation" API token which can be used for GitHub repository access.
// A token is valid for one hour after creation.
githubAppId = '23179'
githubAppCredentialId = 'teampam-ci'
def newApiToken() {
    withEnv(['HTTPS_PROXY=webproxy-internett.nav.no:8088']) {
        withCredentials([file(credentialsId: githubAppCredentialId, variable: 'KEYFILE')]) {
            dir('token') {
                def generatedToken = sh(script: "generate-jwt.sh \$KEYFILE ${githubAppId} | xargs generate-installation-token.sh", returnStdout: true)
                return generatedToken.trim()
            }
        }
    }
}

node {
    def commitHashShort, committer, releaseVersion
    def application = "pam-cv-samtale"
    def dockerRepo = "repo.adeo.no:5443"
    def zone = 'sbs'
    def namespace = 'q0'
    def repo = "navikt"
    GString naiseratorFilePath
    def githubAppToken = newApiToken()

    stage("Checkout") {
        cleanWs()
        withEnv(['HTTPS_PROXY=http://webproxy-internett.nav.no:8088']) {
            // githubAppToken is not a magic secret variable, so mask it manually by disabling shell echo
            // Would be great if withCredentials could be used to mask the value, mark it as secret, or similar
            println("Repository URL is https://x-access-token:****@github.com/${repo}/${application}.git")
            sh(script: "set +x; git clone https://x-access-token:${githubAppToken}@github.com/${repo}/${application}.git .")
        }

        commitHashShort = sh(script: 'git rev-parse --short HEAD', returnStdout: true).trim()
        committer = sh(script: 'git log -1 --pretty=format:"%an"', returnStdout: true).trim()

        releaseVersion = "${env.BUILD_NUMBER}-${commitHashShort}"
        echo "release version: ${releaseVersion}"
        currentBuild.displayName = releaseVersion
    }

    stage("Build") {
        withEnv(['HTTPS_PROXY=http://webproxy-internett.nav.no:8088', 'HTTP_PROXY=http://webproxy-internett.nav.no:8088', 'NO_PROXY=localhost,127.0.0.1,maven.adeo.no', 'NODE_TLS_REJECT_UNAUTHORIZED=0', 'PORT=8081']) {
            sh "npm config set strict-ssl false"
            sh "NODE_TLS_REJECT_UNAUTHORIZED=0 npm ci"
            sh "NODE_TLS_REJECT_UNAUTHORIZED=0 npm run build"
            sh "npm run build:server"
            sh "docker build " +
                "--build-arg HTTPS_PROXY=\"http://webproxy-internett.nav.no:8088\" " +
                "--build-arg HTTP_PROXY=\"http://webproxy-internett.nav.no:8088\" " +
                "-t ${dockerRepo}/${application}:${releaseVersion} ."
        }
    }

    stage("Publish") {
        withCredentials([usernamePassword(credentialsId: 'nexusUploader', usernameVariable: 'NEXUS_USERNAME', passwordVariable: 'NEXUS_PASSWORD')]) {
            sh "docker login -u ${env.NEXUS_USERNAME} -p ${env.NEXUS_PASSWORD} ${dockerRepo} && docker push ${dockerRepo}/${application}:${releaseVersion}"
        }
    }

    stage("Generer nais.yaml") {
        naiseratorFilePath = "${env.WORKSPACE}/naiserator-${releaseVersion}.yml"
        sh "docker pull repo.adeo.no:5443/team-aasmund/nais-template"
        sh "docker run -t -v \$PWD:/tmp -e IMAGE=$dockerRepo/$application:$releaseVersion -e TEMPLATE=preprod-$zone-${namespace}.json repo.adeo.no:5443/team-aasmund/nais-template >> $naiseratorFilePath"
    }

    stage("Deploy til preprod") {
        withCredentials([file(credentialsId: "kubeconfig-preprod-${zone}", variable: 'KUBECONFIG')]) {
            println("Deployer med følgende nais.yaml:")
            sh "cat $naiseratorFilePath"
            dir('kubeconfigs') {
                sh "kubectl apply -n $namespace -f $naiseratorFilePath --wait=true"
                sh "kubectl rollout status -w deployment/$application -n $namespace"
            }
        }
        GString data = """'
                {
                    "environment": "$namespace",
                    "version": "$releaseVersion",
                    "application":"$application",
                    "deployedBy": "Team Aasmund Jenkins-jobb #${env.BUILD_NUMBER}"
                }
            '"""
        sh """curl -X POST --header "Content-Type: application/json" -d $data https://vera.adeo.no/api/v1/deploylog """
    }

    stage("Tag") {
        withEnv(['HTTPS_PROXY=http://webproxy-internett.nav.no:8088']) {
            sh "git tag -a ${releaseVersion} -m ${releaseVersion}"
            sh ("git push -q origin --tags")
        }
    }
}
