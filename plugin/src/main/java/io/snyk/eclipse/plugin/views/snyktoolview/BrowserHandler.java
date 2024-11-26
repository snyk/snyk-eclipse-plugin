package io.snyk.eclipse.plugin.views.snyktoolview;

import java.nio.file.Paths;
import java.util.concurrent.CompletableFuture;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeNode;
import org.eclipse.lsp4e.LSPEclipseUtils;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.browser.ProgressAdapter;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;

import io.snyk.eclipse.plugin.html.HtmlProviderFactory;
import io.snyk.eclipse.plugin.html.StaticPageHtmlProvider;
import io.snyk.eclipse.plugin.utils.ResourceUtils;

public class BrowserHandler {
	private Browser browser;
	private String initScript = "";
	public BrowserHandler(Browser browser) {
		this.browser = browser;
	}
	
	public void initialize() {
		new BrowserFunction(browser, "openInEditor") {
		    @SuppressWarnings("restriction")
			@Override
		    public Object function(Object[] arguments) {
		        if (arguments.length != 5) {
		        	return null;
		        }
	            String filePath = (String) arguments[0];
	            var fileUri = Paths.get(filePath).toUri().toASCIIString();
	            int startLine = Integer.parseInt(arguments[1].toString());
	            int endLine = Integer.parseInt(arguments[2].toString());
	            int startCharacter = Integer.parseInt(arguments[3].toString());
	            int endCharacter = Integer.parseInt(arguments[4].toString());
	
	            Display.getDefault().asyncExec(() -> {
	                try {
	                    Position startPosition = new Position(startLine, startCharacter);
	                    Position endPosition = new Position(endLine, endCharacter);
	                    Range range = new Range(startPosition, endPosition);
	
	                    var location = new Location(fileUri, range);
	                    LSPEclipseUtils.openInEditor(location);
	
	                } catch (Exception e) {
	                    e.printStackTrace();
	                }
	            });
		        return null;
		    }
		};
	
	    browser.addLocationListener(new LocationListener() {
	        @Override
	        public void changing(LocationEvent event) {
	            String url = event.location;
	            if(url.startsWith("http")) {
	            	event.doit = false;
	                Program.launch(url);
	            }
	        }
	
	        @Override
	        public void changed(LocationEvent event) {
	        }
	    });
	    
		browser.addProgressListener(new ProgressAdapter() {
		    @Override
		    public void completed(ProgressEvent event) {
		    	if(!StringUtils.isEmpty(initScript)) {
		    		browser.execute(initScript);	
		    	}
		    }
		});	    
	    initBrowserText();
	}

	public void updateBrowserContent(String text) {
		String htmlContent = generateHtmlContent(text);
		browser.setText(htmlContent);
	}

	public CompletableFuture<Void> updateBrowserContent(TreeNode node) {
		// Generate HTML content based on the selected node
		if (!(node instanceof IssueTreeNode)) return CompletableFuture.completedFuture(null);
		browser.setText(StaticPageHtmlProvider.getInstance().getLoadingHtml());

		return CompletableFuture.supplyAsync(() -> {
			return generateHtmlContent(node);
			})
			.thenAccept(htmlContent -> {
				Display.getDefault().asyncExec(() -> {
					var product = ((ProductTreeNode) node.getParent().getParent()).getProduct();
					var htmlProvider = HtmlProviderFactory.GetHtmlProvider(product);
					var content = htmlProvider.replaceCssVariables(htmlContent);
					initScript = htmlProvider.getInitScript();
					browser.setText(content);
				});
			});
		
	}

	public String generateHtmlContent(TreeNode node) {
		if (node instanceof BaseTreeNode) {
			return ((BaseTreeNode) node).getDetails();
		}
		return "";
	}

	public String generateHtmlContent(String text) {
		return "<html><body<p>" + text + "</p></body></html>";
	}

	public void initBrowserText() {
		browser.setText(StaticPageHtmlProvider.getInstance().getInitHtml());
	}	
}
