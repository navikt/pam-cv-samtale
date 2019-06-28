FROM node:10 as builder

WORKDIR /usr/src/app

COPY src/ ./src
COPY package.json package-lock.json elm.json server.ts tsconfig.json ./

RUN npm --version
RUN npm config set strict-ssl false
RUN npm ci
RUN npm run build
RUN npm run build:server

# Selve imaget:
FROM node:10-alpine

WORKDIR /usr/src/app

COPY --from=builder /usr/src/app/dist/ ./dist
COPY --from=builder /usr/src/app/package.json ./
COPY --from=builder /usr/src/app/package-lock.json ./
COPY --from=builder /usr/src/app/server.js ./

RUN npm config set strict-ssl false
RUN npm ci --production

EXPOSE 8080
CMD ["npm", "run", "start:server"]
