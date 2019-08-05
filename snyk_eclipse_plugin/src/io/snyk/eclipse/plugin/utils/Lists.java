package io.snyk.eclipse.plugin.utils;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Lists {
	
	private Lists() {}
	
	public static <E> List<E> of​(E... elements) {
        List<E> list = Stream.of(elements).collect(Collectors.toList());
        return Collections.unmodifiableList(list);
    }
}
