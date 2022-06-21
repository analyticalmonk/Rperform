FROM archlinux

RUN pacman --noconfirm -Syy r make gcc clang wget cmake lib32-glibc python python-setuptools 

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Executes `entrypoint.sh` when the Docker container starts up
ENTRYPOINT ["/entrypoint.sh"]