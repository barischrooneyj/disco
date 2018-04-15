#+title: disco

* Developing

Install [[https://www.docker.com/get-docker][Docker]], [[https://docs.docker.com/compose][Docker Compose]], and [[https://docs.haskellstack.org/en/stable/README][Stack]]. On macOS you can do:

#+begin_src bash
  brew cask install docker
  brew install docker-machine haskell-stack
#+end_src

Start the Docker daemon:

#+begin_src bash
  open -a docker
#+end_src

Build a Docker image named 'disco':

#+begin_src bash
  ./image.sh
#+end_src

Start some instances of the 'disco' container:

#+begin_src bash
  ./up.sh
#+end_src
