{-Bouncing ball
Mimic the following Python datatype, in Haskell functional style
Implement a move f., for advancing a step and bouncing at borders
ARENA_W, ARENA_H = 320, 240
BALL_W, BALL_H = 20, 20

class Ball:
    def __init__(self, x: int, y: int):
        self._x = x
        self._y = y
        self._dx = 5
        self._dy = 5
    # ...
https://tomamic.github.io/pyodide/?p04_ball.py-}


