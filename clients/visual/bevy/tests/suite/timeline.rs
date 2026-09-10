use hornvale_bevy_view::{FilmClock, PresentationTimeline};
#[test]
fn signed_offsets_ties_and_quantized_endpoints() {
    for (start, end, frames, expected) in [
        (0, -5, 3, vec![0, -2, -3]),
        (7, 8, 2, vec![7, 8]),
        (7, 6, 2, vec![7, 6]),
        (77, 77, 3, vec![77; 3]),
    ] {
        let clock = FilmClock {
            start_ticks: start,
            end_ticks: end,
            frames,
        };
        assert_eq!(
            (0..frames)
                .map(|f| clock.tick_at(f).unwrap())
                .collect::<Vec<_>>(),
            expected
        );
    }
    for end in [-1, 1] {
        assert_eq!(
            FilmClock {
                start_ticks: 0,
                end_ticks: end,
                frames: 300
            }
            .tick_at(299)
            .unwrap(),
            end
        );
    }
}
#[test]
fn boundaries_and_invalid_frames() {
    let c = FilmClock {
        start_ticks: i64::MIN,
        end_ticks: i64::MAX,
        frames: u32::MAX,
    };
    assert_eq!(c.tick_at(0).unwrap(), i64::MIN);
    assert!(c.tick_at(u32::MAX - 1).is_ok());
    assert!(c.tick_at(u32::MAX).is_err());
    assert!(FilmClock { frames: 0, ..c }.tick_at(0).is_err());
    assert_eq!(
        FilmClock {
            start_ticks: i64::MAX,
            end_ticks: i64::MIN,
            frames: 2
        }
        .tick_at(1)
        .unwrap(),
        -1
    );
}
#[test]
fn direct_seek_equals_sequential_even_with_paused_simulation() {
    for end_ticks in [77, 100000, -100000] {
        let c = FilmClock {
            start_ticks: 77,
            end_ticks,
            frames: 300,
        };
        let mut sequential = PresentationTimeline::new(c.clone()).unwrap();
        for frame in 0..=150 {
            sequential.seek(frame).unwrap();
        }
        assert_eq!(
            PresentationTimeline::new(c).unwrap().seek(150).unwrap(),
            sequential.sample().unwrap()
        );
    }
}
