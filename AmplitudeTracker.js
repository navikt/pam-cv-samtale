import amplitude from "amplitude-js";

const amplitudeTracker = (amplitudeToken, userId = null) => {
    let amplitudeClient;
    if (amplitudeToken) {
        amplitudeClient = amplitude.getInstance();
        amplitudeClient.init(
            amplitudeToken, userId   , {
                apiEndpoint: 'amplitude.nav.no/collect',
                batchEvents: false,
                includeReferrer: true,
                includeUtm: true,
                saveEvents: false
            }
        );
    }
    return amplitudeClient;
};

const logAmplitudeEvent = (sessionID, eventName, eventPayload, amplitudeToken) => {
    amplitudeTracker(amplitudeToken, sessionID).logEvent(eventName, eventPayload);
};

export default logAmplitudeEvent;
