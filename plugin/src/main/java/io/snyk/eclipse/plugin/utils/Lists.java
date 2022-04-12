package io.snyk.eclipse.plugin.utils;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Lists {

  private Lists() {
  }

  @SafeVarargs
  public static <E> List<E> of(E... elements) {
    return Stream.of(elements).collect(Collectors.toUnmodifiableList());
  }
}
