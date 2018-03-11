///////////////////////////////////////////////////////////////////////////////
// ADDINS
///////////////////////////////////////////////////////////////////////////////

#tool "nuget:?package=GitVersion.CommandLine&version=3.6.5"
#addin "nuget:?package=Cake.Figlet&version=1.0.0"

///////////////////////////////////////////////////////////////////////////////
// ARGUMENTS
///////////////////////////////////////////////////////////////////////////////

var target = Argument("target", "Default");
var configuration = Argument("configuration", "Release");

///////////////////////////////////////////////////////////////////////////////
// CONFIGURATION
///////////////////////////////////////////////////////////////////////////////

var projectName = "FLexer";
var projectFile = File("./src/FLexer.Core/FLexer.Core.fsproj");
var testProjectFile = File("./src/FLexer.Tests/FLexer.Tests.fsproj");
var solutionFile = File("./src/FLexer.sln");
var nugetVersion = "";
var semVer = "";
var nugetApiKey = Argument("NUGET_API_KEY", EnvironmentVariable("NUGET_API_KEY"));

var branch = EnvironmentVariable("APPVEYOR_REPO_BRANCH") ?? "";
var isMasterBranch = branch.ToUpper().Contains("MASTER");
var isTagged = (EnvironmentVariable("APPVEYOR_REPO_TAG") ?? "").Contains("true");
var tagName = EnvironmentVariable("APPVEYOR_REPO_TAG_NAME") ?? "";

var local = BuildSystem.IsLocalBuild;
var isRunningOnAppVeyor = AppVeyor.IsRunningOnAppVeyor;
var isPullRequest = AppVeyor.Environment.PullRequest.IsPullRequest;

///////////////////////////////////////////////////////////////////////////////
// SETUP / TEARDOWN
///////////////////////////////////////////////////////////////////////////////

Setup(ctx =>
{
   // Executed BEFORE the first task.
    Information(Figlet("FLexer"));
    Information("");
    Information("Branch: " + (branch ?? ""));
    Information("tagName: " + tagName);
    Information("IsTagged: " + (isTagged ? "true": "false"));
    Information("IsPullRequest: " + (isPullRequest ? "true": "false"));
});

Teardown(ctx =>
{
   // Executed AFTER the last task.
   Information("Finished running tasks.");
});

///////////////////////////////////////////////////////////////////////////////
// TASKS
///////////////////////////////////////////////////////////////////////////////

Task("clean")
    .Does(() =>
{
    CleanDirectories("./build");
    CleanDirectories("./**/bin");
    CleanDirectories("./**/obj");
    CreateDirectory("./build");
});

Task("get-version")
    .Does(() =>
{
    var version = GitVersion(new GitVersionSettings());

    semVer = version.SemVer;
    nugetVersion = version.NuGetVersionV2;

    Information("SemVer: " + semVer);
    Information("NuGet: " + nugetVersion);
});

Task("set-version")
    .Does(() =>
{
	var version = GitVersion(new GitVersionSettings {
        UpdateAssemblyInfo = true
    });

	semVer = version.SemVer;
	nugetVersion = version.NuGetVersionV2;

    // ReplaceProperty("Version", nugetVersion);
    // ReplaceProperty("Copyright", "Copyright (c) " + DateTime.Now.Year);

	Information("Nuget Version: " + nugetVersion);
    Information("SemVer: " + semVer);
});

Task("restore-nuget-packages")
    .IsDependentOn("clean")
    .Does(() =>
{
    Information("Restoring packages for {0}", solutionFile);

    DotNetCoreRestore(projectFile);
    DotNetCoreRestore(projectFile);
});

Task("test")
    .Does(() =>
{
    DotNetCoreTest(testProjectFile);
});

Task("pack")
    .IsDependentOn("set-version")
    .IsDependentOn("clean")
	.IsDependentOn("restore-nuget-packages")
    .IsDependentOn("test")
.Does(() =>
{
    var msBuildSettings = new DotNetCoreMSBuildSettings();
    msBuildSettings.Properties["PackageVersion"] = new[] { semVer };
    msBuildSettings.Properties["TargetFrameworks"] = new[] { "netstandard2.0" };

    var settings = new DotNetCorePackSettings
    {
        Configuration = "Release",
        OutputDirectory = "./build/",
        MSBuildSettings = msBuildSettings
    };

    DotNetCorePack("./src/FLexer.Core/FLexer.Core.fsproj", settings);
});

Task("push")
    .IsDependentOn("pack")
    .WithCriteria(() => isRunningOnAppVeyor && isMasterBranch && !isPullRequest && isTagged)
.Does(() =>
{
    var files = GetFiles("./build/*.nupkg");
    foreach(var nupkgFile in files)
    {
        Information("File: {0}", nupkgFile);
        var settings = new DotNetCoreNuGetPushSettings
        {
            ApiKey = nugetApiKey,
            Source = "https://api.nuget.org/v3/index.json"
        };

        DotNetCoreNuGetPush(nupkgFile.ToString(), settings);
    }
});


Task("Default")
    .IsDependentOn("push");

RunTarget(target);