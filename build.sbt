name := "CodeWars"

version := "0.1"

scalaVersion := "2.13.1"
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked"
)

libraryDependencies ++= Seq(
  "org.scalacheck"           %% "scalacheck"               % "1.14.0"    % Test,
  "org.scalactic"            %% "scalactic"                % "3.1.0"     % Test,
  "org.scalatest"            %% "scalatest"                % "3.1.0"     % Test,
  "com.novocode"             % "junit-interface"           % "0.11"      % Test
)
