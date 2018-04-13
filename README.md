#+title: disco

* Developing

Install [Stack](https://docs.haskellstack.org/en/stable/README/),
[Docker](https://www.docker.com/get-docker) and [Docker
Compose](https://docs.docker.com/compose/). On macOS you can do:

#+begin_src bash
brew cask install docker
brew install docker-machine haskell-stack
#+end_src

Start the Docker daemon:

#+begin_src bash
  open -a docker
#+end_src

Image a Docker container named 'disco':

#+begin_src bash
stack image container --build  # or ./image.sh
#+end_src

Start some instances of the 'disco' container:

#+begin_src bash
docker-compose up --scale disco=5  # or ./up.sh
#+end_src
