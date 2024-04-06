use crate::types::prelude::*;

/// The probability that a given star subsystem will be binary.
///
/// This probability might be slightly lower than actual.
pub const BINARY_STAR_PROBABILITY: f64 = 0.25;

/// Kilograms per solar mass.
pub const KG_PER_SOLAR_MASS: MassInKg = MassInKg(1.989E30);

/// Kilograms per Jupiter mass.
pub const KG_PER_JUPITER_MASS: MassInKg = MassInKg(5.26704E28);

/// Kilograms per earth mass.
pub const KG_PER_EARTH_MASS: MassInKg = MassInKg(5.972E24);

/// Kilograms per lunar mass.
pub const KG_PER_LUNAR_MASS: MassInKg = MassInKg(7.34767309E22);

/// Kilometers per AU.
pub const KM_PER_AU: LengthInKm = LengthInKm(149_597_870.7);

/// Kilometers per light year.
pub const KM_PER_LYR: LengthInKm = LengthInKm(9.461E12);

/// Astronomical units per light year.
pub const AU_PER_LYR: LengthInAu = LengthInAu(63241.077);

/// Kilometers in RJupiter.
pub const KM_PER_JUPITER_RADIUS: LengthInKm = LengthInKm(69_911.0);

/// Kilometers in RadiusOfEarth.
pub const KM_PER_EARTH_RADIUS: LengthInKm = LengthInKm(6371.0);

/// Kilometers in RLuna.
pub const KM_PER_LUNA_RADIUS: LengthInKm = LengthInKm(1737.1);

/// Kilometers in RSol.
pub const KM_PER_SOL_RADIUS: LengthInKm = LengthInKm(696_340.0);

/// Diameter of the Earth in Kilometers.
pub const KM_PER_EARTH_DIAMETER: LengthInKm = LengthInKm(2.0 * KM_PER_EARTH_RADIUS.0);

/// Minimum mass, in MassOfLuna.
pub const MINIMUM_MOON_MASS: MassOfLuna = MassOfLuna(0.05);

/// Maximum mass, in MassOfLuna.
pub const MAXIMUM_MOON_MASS: MassOfLuna = MassOfLuna(1.00);

/// Minimum albedo (unitless).
pub const MINIMUM_MOON_ALBEDO: f64 = 0.25;

/// Maximum albedo (unitless).
pub const MAXIMUM_MOON_ALBEDO: f64 = 1.00;

/// Ratio of Luna's share of the Earth-Luna gravitational parameter.
pub const LUNA_GRAVITATIONAL_PARAMETER_SHARE: f64 = 0.0123;

/// Ratio of Earth mass to solar mass.
pub const EARTH_MASS_PER_SOLAR_MASS: MassOfEarth = MassOfEarth(333_000.0);

/// Ratio of Jupiter mass to solar mass.
pub const JUPITER_MASS_PER_SOLAR_MASS: MassOfJupiter = MassOfJupiter(1048.0);

/// Ratio of Earth mass to Jupiter mass.
pub const EARTH_MASS_PER_JUPITER_MASS: MassOfEarth = MassOfEarth(317.8);

/// Ratio of Luna mass to Earth mass.
pub const LUNA_MASS_PER_EARTH_MASS: MassOfLuna = MassOfLuna(81.3);

/// Jupiter's density.
pub const DENSITY_OF_JUPITER: DensityInGramsPerCm3 = DensityInGramsPerCm3(1.33);

/// Earth's density.
pub const DENSITY_OF_EARTH: DensityInGramsPerCm3 = DensityInGramsPerCm3(5.51);

/// Luna's density.
pub const DENSITY_OF_LUNA: DensityInGramsPerCm3 = DensityInGramsPerCm3(3.34);

/// Sol's density.
pub const DENSITY_OF_SOL: DensityInGramsPerCm3 = DensityInGramsPerCm3(1.41);

/// Hours per day.
pub const EARTH_HOURS_PER_DAY: TimeInEarthHours = TimeInEarthHours(24.0);

/// Days per year.
pub const EARTH_DAYS_PER_YEAR: TimeInEarthDays = TimeInEarthDays(365.25);

/// LSol -> Ergs/sec
pub const ERGS_PER_SEC_PER_LSOL: f64 = 3.846E33;

/// LSol -> Joules/sec
pub const JOULES_PER_SEC_PER_LSOL: f64 = 3.846E26;

/// The radius of our stellar neighborhood.
///
/// This may be flexible or changed at some point, but for the time being I'm
/// thinking about fairly conventional fantasy systems where interstellar
/// travel isn't a thing.
///
/// Measured in Ly, or light years.
pub const STELLAR_NEIGHBORHOOD_RADIUS: LengthInLyr = LengthInLyr(10.0);

/// The stellar density of our (stellar) neighborhood.
///
/// As above, this is currently set to be fairly conventional.
///
/// Measured in s/ly^3, or stars per cubic light year.
pub const STELLAR_NEIGHBORHOOD_DENSITY: f64 = 0.004;

/// The minimum number of moons we'll generate for a terrestrial planet.
pub const MINIMUM_TERRESTRIAL_MOONS: usize = 0;

/// The maximum number of moons we'll generate for a terrestrial planet.
pub const MAXIMUM_TERRESTRIAL_MOONS: usize = 2;

/// The minimum number of moons we'll generate for a gas giant plant.
pub const MINIMUM_GAS_GIANT_MOONS: usize = 8;

/// The maximum number of moons we'll generate for a gas giant plant.
pub const MAXIMUM_GAS_GIANT_MOONS: usize = 20;

/// Minimum number of satellite systems to generate.
pub const MINIMUM_SATELLITE_SYSTEMS: usize = 0;

/// Maximum number of satellite systems to generate.
pub const MAXIMUM_SATELLITE_SYSTEMS: usize = 12;

/// Escape velocity from the Earth's surface.
pub const ESCAPE_VELOCITY_EARTH: SpeedInKmPerSec = SpeedInKmPerSec(11.2);

/// Gravitational acceleration on the Earth's surface.
pub const GRAVITATIONAL_ACCELERATION_EARTH: AccelerationInMetersPerSec2 = AccelerationInMetersPerSec2(9.807);

/// Gravitational acceleration on Luna's surface.
pub const GRAVITATIONAL_ACCELERATION_LUNA: AccelerationInMetersPerSec2 = AccelerationInMetersPerSec2(1.625);

/// Oxygen weight in kg/mol.
pub const OXYGEN_WEIGHT: KgPerMol = KgPerMol(0.032);

/// Carbon dioxide weight in kg/mol.
pub const CO2_WEIGHT: KgPerMol = KgPerMol(0.044);

/// Argon weight in kg/mol.
pub const ARGON_WEIGHT: KgPerMol = KgPerMol(0.040);

/// Nitrogen weight in kg/mol.
pub const NITROGEN_WEIGHT: KgPerMol = KgPerMol(0.028);

/// Minimum mass for a terrestrial planet, in MassOfEarth.
pub const MINIMUM_TERRESTRIAL_PLANET_MASS: MassOfEarth = MassOfEarth(0.1);

/// Maximum mass for a terrestrial planet, in MassOfEarth.
pub const MAXIMUM_TERRESTRIAL_PLANET_MASS: MassOfEarth = MassOfEarth(10.0);

/// Minimum mass for a habitable planet, in MassOfEarth.
/// Raised from 0.10 because that sounds ludicrous.
pub const MINIMUM_HABITABLE_TERRESTRIAL_PLANET_MASS: MassOfEarth = MassOfEarth(0.75);

/// Maximum mass for a habitable planet, in MassOfEarth.
/// Lowered because 3.5 just sounds extreme to me.
pub const MAXIMUM_HABITABLE_TERRESTRIAL_PLANET_MASS: MassOfEarth = MassOfEarth(1.50);

/// Minimum habitable rotational period, in TimeInEarthDays.
/// Shorter than ~6 hours gets rotationally intense.
pub const MINIMUM_HABITABLE_ROTATIONAL_PERIOD: TimeInEarthDays = TimeInEarthDays(0.25);

/// Maximum habitable rotational period, in TimeInEarthDays.
/// Longer than ~48 hours gets rotationally weird.
pub const MAXIMUM_HABITABLE_ROTATIONAL_PERIOD: TimeInEarthDays = TimeInEarthDays(2.0);

/// Minimum orbitable eccentricity (unitless).
pub const MINIMUM_ORBITAL_ECCENTRICITY: f64 = 0.0;

/// Maximum orbitable eccentricity (unitless).
pub const MAXIMUM_ORBITAL_ECCENTRICITY: f64 = 0.10;

/// Maximum habitable orbitable eccentricity (unitless).
pub const MINIMUM_HABITABLE_ORBITAL_ECCENTRICITY: f64 = MINIMUM_ORBITAL_ECCENTRICITY;

/// Maximum habitable orbitable eccentricity (unitless).
pub const MAXIMUM_HABITABLE_ORBITAL_ECCENTRICITY: f64 = 0.02;

/// Minimum Bond albedo (unitless).
pub const MINIMUM_BOND_ALBEDO: f64 = 0.01;

/// Maximum Bond albedo (unitless).
pub const MAXIMUM_BOND_ALBEDO: f64 = 1.00;

/// Minimum Bond albedo (unitless).
pub const MINIMUM_HABITABLE_BOND_ALBEDO: f64 = 0.11;

/// Maximum Bond albedo (unitless).
pub const MAXIMUM_HABITABLE_BOND_ALBEDO: f64 = 0.50;

/// Stefan-Boltzmann constant (W/m²K⁴).
pub const STEFAN_BOLTZMANN_CONSTANT: f64 = 0.00005670374419;

/// Too damned cold.
pub const MINIMUM_HABITABLE_TEMPERATURE: TemperatureInKelvin = TemperatureInKelvin(273.0);

/// Too damned hot.
pub const MAXIMUM_HABITABLE_TEMPERATURE: TemperatureInKelvin = TemperatureInKelvin(323.0);

/// Too damned floaty.
pub const MINIMUM_HABITABLE_GRAVITY: GravityOfEarth = GravityOfEarth(0.5);

/// Too damned hard to get out of bed.
pub const MAXIMUM_HABITABLE_GRAVITY: GravityOfEarth = GravityOfEarth(1.5);

/// The strength of the greenhouse effect.
pub const GREENHOUSE_EFFECT: f64 = 0.5841;

/// Prelude.
pub mod prelude {
  pub use super::*;
}
