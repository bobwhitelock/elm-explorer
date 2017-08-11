
import hello from 'hellojs'

import './main.css';
import { Main } from './Main.elm';


window.hello = hello

hello.init({
  github: '49d32710bb28856b98b0',
}, {redirect_uri: 'http://127.0.0.1'})

hello.on(
  'auth.login',
  (auth) => console.log('Authenticated!', auth) ,
  (error) => console.log('Something went wrong:', error)
)

hello.on(
  'auth.logout',
  (auth) => console.log('Logged out!', auth) ,
  (error) => console.log('Something went wrong:', error)
)

Main.embed(document.getElementById('root'));
