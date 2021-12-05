plugins {
    id("org.jetbrains.kotlin.jvm") version "1.6.0"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(platform("org.jetbrains.kotlin:kotlin-bom"))
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.6.0-RC")
}

application {
    mainClass.set("com.eddsteel.advent.twentyone.MainKt")
}

dependencyLocking {
    lockMode.set(LockMode.STRICT)
}
