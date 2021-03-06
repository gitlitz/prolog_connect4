#!/usr/bin/python

# coding=UTF8

# Python Connect4 game (https://github.com/mevdschee/python-connect4.git)
# Author: Maurits van der Schee <maurits@vdschee.nl>

import argparse
import os
import sys
import copy
import time
import threading
import subprocess
from Tkinter import Tk, Button, Frame, Canvas
from tkFont import Font

class Board:
  nodes = {}

  def __init__(self,other=None):
    self.player = 'X'
    self.opponent = 'O'
    self.empty = '.'
    self.width = 7
    self.height = 6
    self.fields = {}
    for y in range(self.height):
      for x in range(self.width):
        self.fields[x,y] = self.empty
    # copy constructor
    if other:
      self.__dict__ = copy.deepcopy(other.__dict__)

  def move(self,x):
    board = Board(self)
    for y in range(board.height):
      if board.fields[x,y] == board.empty:
        board.fields[x,y] = board.player
        break
    board.player,board.opponent = board.opponent,board.player
    return board

  def tied(self):
    for (x,y) in self.fields:
      if self.fields[x,y]==self.empty:
        return False
    return True

  def won(self):
    # horizontal
    for y in range(self.height):
      winning = []
      for x in range(self.width):
        if self.fields[x,y] == self.opponent:
          winning.append((x,y))
          if len(winning) == 4:
            return winning
        else:
          winning = []
    # vertical
    for x in range(self.width):
      winning = []
      for y in range(self.height):
        if self.fields[x,y] == self.opponent:
          winning.append((x,y))
          if len(winning) == 4:
            return winning
        else:
          winning = []
    # diagonal
    winning = []
    for cx in range(self.width-1):
      sx,sy = max(cx-2,0),abs(min(cx-2,0))
      winning = []
      for cy in range(self.height):
        x,y = sx+cy,sy+cy
        if x<0 or y<0 or x>=self.width or y>=self.height:
          continue
        if self.fields[x,y] == self.opponent:
          winning.append((x,y))
          if len(winning) == 4:
            return winning
        else:
          winning = []
    # other diagonal
    winning = []
    for cx in range(self.width-1):
      sx,sy = self.width-1-max(cx-2,0),abs(min(cx-2,0))
      winning = []
      for cy in range(self.height):
        x,y = sx-cy,sy+cy
        if x<0 or y<0 or x>=self.width or y>=self.height:
          continue
        if self.fields[x,y] == self.opponent:
          winning.append((x,y))
          if len(winning) == 4:
            return winning
        else:
          winning = []
    # default
    return None

  def __str__(self):
    string = ''
    for y in range(self.height):
      for x in range(self.width):
        string+=' '+self.fields[x,self.height-1-y]
      string+="\n"
    return string

class GUI:

  def __init__(self, depth):
    self.depth = depth
    self._init_app()
    self._init_ai()
    self.update()

  def _init_app(self):
    self.app = Tk()
    self.app.title('Connect4')
    self.app.resizable(width=False, height=False)
    self.board = Board()
    self.buttons = {}
    self.frame = Frame(self.app, borderwidth=1, relief="raised")
    self.tiles = {}
    for x in range(self.board.width):
      handler = lambda x=x: self.move(x)
      button = Button(self.app, command=handler, font=Font(family="Helvetica", size=14), text=x+1)
      button.grid(row=0, column=x, sticky="WE")
      self.buttons[x] = button
    self.frame.grid(row=1, column=0, columnspan=self.board.width)
    for x,y in self.board.fields:
      tile = Canvas(self.frame, width=60, height=50, bg="navy", highlightthickness=0)
      tile.grid(row=self.board.height-1-y, column=x)
      self.tiles[x,y] = tile
    handler = lambda: self.reset()
    self.restart = Button(self.app, command=handler, text='reset')
    self.restart.grid(row=2, column=0, columnspan=self.board.width, sticky="WE")

  def _init_ai(self):
    ai_path = os.path.join(os.path.dirname(__file__), 'conn4.pl')
    # for widnows: self.ai = subprocess.Popen([r"C:\Program Files\swipl\bin\swipl.exe", ai_path], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    print(ai_path)
    self.ai = subprocess.Popen(["swipl", ai_path, str(self.depth)], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

  def reset(self):
    self.ai.terminate()
    self._init_ai()
    self.board = Board()
    self.update()

  def move(self,x):
    self.app.config(cursor="watch")
    self.app.update()
    try:
      # me
      self.ai.stdin.write(str(x + 1) + '\n')
      print self.ai.stdout.readline().strip()
      self.board = self.board.move(x)
      if self.update():
        return
      # him
      self.ai.stdin.write('0\n')
      y = self.ai.stdout.readline().strip()
      self.board = self.board.move(int(y[3]) - 1)
      self.update()
      print y
    finally:
      self.app.config(cursor="")

  def update(self):
    for (x,y) in self.board.fields:
      text = self.board.fields[x,y]
      if (text=='.'):
        self.tiles[x,y].create_oval(10, 5, 50, 45, fill="black", outline="blue", width=1)
      if (text=='X'):
        self.tiles[x,y].create_oval(10, 5, 50, 45, fill="yellow", outline="blue", width=1)
      if (text=='O'):
        self.tiles[x,y].create_oval(10, 5, 50, 45, fill="red", outline="blue", width=1)
    for x in range(self.board.width):
      if self.board.fields[x,self.board.height-1]==self.board.empty:
        self.buttons[x]['state'] = 'normal'
      else:
        self.buttons[x]['state'] = 'disabled'
    winning = self.board.won()
    if winning:
      self.ai.terminate()
      for x,y in winning:
        self.tiles[x,y].create_oval(25, 20, 35, 30, fill="black")
      for x in range(self.board.width):
        self.buttons[x]['state'] = 'disabled'
    return winning

  def mainloop(self):
    self.app.mainloop()

def parse_args():
  parser = argparse.ArgumentParser(description="Run a 4 in a row game, with ai written in prolog")
  parser.add_argument("--depth", help="Set the maximum depth search for the ai. This value must be at least 1", 
        default=4, type=int)
  depth = parser.parse_args().depth

  if depth < 1:
      print("error: depth must be at least 1")
      sys.exit(1)
  return depth

if __name__ == '__main__':
  depth = parse_args()
  gui = GUI(depth)
  try:
    gui.mainloop()
  finally:
    gui.ai.terminate()
