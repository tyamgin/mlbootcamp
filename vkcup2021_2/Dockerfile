FROM continuumio/anaconda3:latest

COPY *.py .

#COPY data /tmp/data

CMD mkdir -p /var
CMD mkdir -p /var/log
CMD python main.py --data-dir /tmp/data --res-path /var/log/result \
 && head -20 /var/log/result


# Для запуска:
# sudo sudo docker build -t vkcup . && docker run -t vkcup

# Для отправки:
# docker tag vkcup stor.highloadcup.ru/vkcup21_age/yellow_jaguar
# docker push stor.highloadcup.ru/vkcup21_age/yellow_jaguar

# Скрипт отправки:
# ./submit.sh