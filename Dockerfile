FROM navikt/node-express:12.2.0

WORKDIR /usr/src/app

COPY dist/ ./dist
COPY package.json package-lock.json server.js  ./

RUN npm config set strict-ssl false
RUN npm ci --production

EXPOSE 8080
CMD ["npm", "run", "start:server"]
