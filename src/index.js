import hello from 'hellojs'

import './main.css'
import { Main } from './Main.elm'
import packages from '../data/packages.json'

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

let accessToken, redirectUri
if (process.env.NODE_ENV == 'development') {
  accessToken = '49d32710bb28856b98b0'
  redirectUri = 'http://127.0.0.1'
} else {
  accessToken = '143fcf6817394a7cf33f'
  redirectUri = 'https://elm-explorer.netlify.com'
}

hello.init({ github: accessToken }, { redirect_uri: redirectUri })

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
