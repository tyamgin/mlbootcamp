FROM continuumio/anaconda3:latest

RUN pip3 install 'lightgbm==3.2.1'

COPY *.py .
COPY models models
COPY data data

COPY data /tmp/data

CMD mkdir -p /var
CMD mkdir -p /var/log
CMD python main.py --test-data-dir /tmp/data --train-data-dir ./data --res-path /var/log/result --work-dir . \
 && head -20 /var/log/result

# pip3 install sklearn

# Для запуска:
# sudo sudo docker build -t vkcup . && docker run -t vkcup

# Для отправки:
# docker tag vkcup stor.highloadcup.ru/vkcup21_age/yellow_jaguar
# docker push stor.highloadcup.ru/vkcup21_age/yellow_jaguar

# Скрипт отправки:
# ./submit.sh