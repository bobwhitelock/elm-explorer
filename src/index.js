import hello from 'hellojs'

import './main.css'
import { Main } from './Main.elm'
import packages from './packages.json'

const validSession = session => {
  const currentTime = new Date().getTime() / 1000
  return session && session.access_token && session.expires > currentTime
}

const existingAccessToken = () => {
  const session = hello('github').getAuthResponse()
  return validSession(session) ? session.access_token : null
}

const flags = {
  packages: packages,
  accessToken: existingAccessToken(),
}
const app = Main.embed(document.getElementById('root'), flags)

window.hello = hello

hello.init(
  { github: '49d32710bb28856b98b0' },
  { redirect_uri: 'http://127.0.0.1' },
)

hello.on(
  'auth.login',
  auth => {
    app.ports.githubOauthSuccess.send(auth.authResponse.access_token)
    console.log('Authenticated!', auth)
  },
  error => console.log('Something went wrong:', error),
)

hello.on(
  'auth.logout',
  auth => console.log('Logged out!', auth),
  error => console.log('Something went wrong:', error),
)
