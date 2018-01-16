import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import java.net.URLEncoder;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Invocation.Builder;

public class SnykAPI {
    private final String POM_XML_CONTENT_PLACEHOLDER = "{{POM_XML_CONTENT}}";

    private String authToken = "";
    private String apiEndpoing = "https://dev.snyk.io/api/v1/test/maven";
    private String mvnRepository = "https://repo1.maven.org/maven2";
    private Client client;

    public SnykAPI(String authToken) {
        this.client = ClientBuilder.newClient();
        this.authToken = authToken;
    }

    public SnykAPI(String authToken, String mvnRepository) {
        this.client = ClientBuilder.newClient();
        this.authToken = authToken;
        this.mvnRepository = mvnRepository;
    }

    public SnykAPI(String authToken, String mvnRepository, String apiEndpoing) {
        this.client = ClientBuilder.newClient();
        this.authToken = authToken;
        this.apiEndpoing = apiEndpoing;
        this.mvnRepository = mvnRepository;
    }

    public String TestPublicPackageByGID(String groupId, String artifactId, String version) throws FailedAPIRequestExecption {
        String url = this.apiEndpoing + "/" + groupId
                + "/" + artifactId
                + "/" + version
                + "?repository=" + this.mvnRepository;

        Response response = this.prepareAPICall(url).get();
        String responseBody = response.readEntity(String.class);
        int status = response.getStatus();

        if (status != 200) {
            throw new FailedAPIRequestExecption("Request returned " + status + "\n\n Body:\n\n" + responseBody);
        }

        return responseBody;
    }

    public String TestPOMFile(String pathToFile) throws FailedAPIRequestExecption, IOException {
        String url = this.apiEndpoing  + "?repository=" + this.mvnRepository;
        String payload = "{\"encoding\":\"plain\",\"files\":{\"target\":{\"contents\":\"" + POM_XML_CONTENT_PLACEHOLDER + "\"}}}";
        FileReader fr = new FileReader(pathToFile);
        BufferedReader br = new BufferedReader(fr);

        String currentLine;
        String pomXMLContent = "";

        while ((currentLine = br.readLine()) != null) {
            pomXMLContent = pomXMLContent.concat(currentLine + "\\n");
        }

        pomXMLContent = pomXMLContent.replace("\"", "\\\"");
        payload = payload.replace(POM_XML_CONTENT_PLACEHOLDER, pomXMLContent);
        System.out.println(payload);

        Response response = this.prepareAPICall(url).post(Entity.json(payload));
        String responseBody = response.readEntity(String.class);
        int status = response.getStatus();

        if (status != 200) {
            throw new FailedAPIRequestExecption("Request returned " + status + "\n\n Body:\n\n" + responseBody);
        }

        return responseBody;
    }

    private Builder prepareAPICall(String url) {
        return client.target(url)
                .request(MediaType.APPLICATION_JSON_TYPE)
                .header("Authorization", "token " + this.authToken);
    }
}
