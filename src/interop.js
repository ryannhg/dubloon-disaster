export const flags = () => {
  let highscores = JSON.parse(localStorage.getItem('highscores') || null) || []

  return {
    highscores
  }
}

export const onReady = ({ app }) => {
  if (app.ports.outgoing) {
    app.ports.outgoing.subscribe(({ tag, payload }) => {
      switch (tag) {
        case 'SEND_HIGHSCORE':
          let { score } = payload
          let highscores = JSON.parse(localStorage.getItem('highscores') || null) || []

          localStorage.setItem('highscores', JSON.stringify(highscores.concat([score])))
          return payload.score
      }
    })
  }
}